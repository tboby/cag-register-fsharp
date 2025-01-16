using System;
using System.Net.Http;
using System.Threading.Tasks;
using HtmlAgilityPack;
using System.Linq;
using Microsoft.Data.Sqlite;
using UglyToad.PdfPig;
using System.Text.RegularExpressions;
using SQLite;
using System.Security.Cryptography;
using System.Threading;
using CommandLine;
using System.IO;

public class Options
{
    [Option('m', "mode", Required = true, HelpText = "Operation mode: 'scrape' for web scraping, 'process' for PDF processing, or 'both' for both operations")]
    public string Mode { get; set; }

    [Option('d', "db", Required = false, Default = "minutes.db", HelpText = "Path to SQLite database")]
    public string DbPath { get; set; }

    [Option('o', "output", Required = false, Default = "downloads", HelpText = "Directory for downloaded PDFs")]
    public string DownloadDir { get; set; }
}

class Program
{
    static async Task Main(string[] args)
    {
        await Parser.Default.ParseArguments<Options>(args)
            .WithParsedAsync(async options =>
            {
                try
                {
                    // Create download directory if it doesn't exist
                    if (!Directory.Exists(options.DownloadDir))
                    {
                        Directory.CreateDirectory(options.DownloadDir);
                    }

                    if (options.Mode == "scrape" || options.Mode == "both")
                    {
                        var scraper = new MinutesScraper(options.DbPath);
                        await scraper.ScrapeMinutes();
                        Console.WriteLine("Minutes scraping completed successfully.");
                    }

                    if (options.Mode == "process" || options.Mode == "both")
                    {
                        var processor = new PdfProcessor(options.DbPath, options.DownloadDir);
                        await processor.ProcessAllPdfs();
                        Console.WriteLine("PDF processing completed successfully.");
                        processor.Dispose();
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Error occurred: {ex.Message}");
                    Environment.Exit(1);
                }
            });
    }
}

[Table("minutes")]
public class MinuteRecord
{
    [PrimaryKey, AutoIncrement]
    public int Id { get; set; }
    public int YearId { get; set; }
    public string Title { get; set; }
    public string Url { get; set; }
    public DateTime CreatedAt { get; set; }
}

[Table("ProcessedPdfs")]
public class ProcessedPdf
{
    [PrimaryKey]
    public string Url { get; set; }
    public string FileHash { get; set; }
    public DateTime ProcessedDate { get; set; }
}

[Table("CagReferences")]
public class CagReference
{
    [PrimaryKey, AutoIncrement]
    public int Id { get; set; }
    [Indexed]
    public string PdfUrl { get; set; }
    [Indexed]
    public string CagId { get; set; }
    public string PageRanges { get; set; }
    public DateTime ProcessedDate { get; set; }
}

public class MinutesScraper
{
    private readonly HttpClient _client;
    private const string BaseUrl = "https://www.hra.nhs.uk/about-us/committees-and-services/confidentiality-advisory-group/cag-group-meetings-and-minutes/";
    private readonly string _dbPath;

    public MinutesScraper(string dbPath = "minutes.db")
    {
        _client = new HttpClient();
        _dbPath = dbPath;
        _client.DefaultRequestHeaders.Add("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36");
        InitializeDatabase();
    }

    private void InitializeDatabase()
    {
        // Create tables using raw SQL first (for years and minutes)
        using var connection = new SqliteConnection($"Data Source={_dbPath}");
        connection.Open();

        using var command = connection.CreateCommand();
        command.CommandText = @"
            CREATE TABLE IF NOT EXISTS years (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                year TEXT NOT NULL,
                url TEXT NOT NULL,
                type TEXT NOT NULL,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            );

            CREATE TABLE IF NOT EXISTS minutes (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                yearId INTEGER NOT NULL,
                title TEXT NOT NULL,
                url TEXT NOT NULL,
                createdAt DATETIME DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY (yearId) REFERENCES years (id)
            );

            CREATE TABLE IF NOT EXISTS ProcessedPdfs (
                url TEXT PRIMARY KEY,
                fileHash TEXT NOT NULL,
                processedDate DATETIME NOT NULL
            );

            CREATE TABLE IF NOT EXISTS CagReferences (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                pdfUrl TEXT NOT NULL,
                cagId TEXT NOT NULL,
                pageRanges TEXT NOT NULL,
                processedDate DATETIME NOT NULL
            );";
        command.ExecuteNonQuery();
    }

    private string GetAbsoluteUrl(string relativeUrl)
    {
        if (string.IsNullOrEmpty(relativeUrl))
            return string.Empty;

        if (relativeUrl.StartsWith("http"))
            return relativeUrl;

        return new Uri(new Uri(BaseUrl), relativeUrl).ToString();
    }

    public async Task ScrapeMinutes()
    {
        try
        {
            var mainPageHtml = await _client.GetStringAsync(BaseUrl);
            var doc = new HtmlDocument();
            doc.LoadHtml(mainPageHtml);

            using var connection = new SqliteConnection($"Data Source={_dbPath}");
            connection.Open();
            using var transaction = connection.BeginTransaction();

            try
            {
                var yearLinks = doc.DocumentNode
                    .SelectNodes("//h2[contains(text(), 'Minutes of previous CAG meetings')]/following-sibling::ul[1]//a")
                    ?.Select(a => new
                    {
                        Url = GetAbsoluteUrl(a.GetAttributeValue("href", "")),
                        Year = a.InnerText.Trim(),
                        Type = "regular"
                    })
                    .ToList();

                var precedentLinks = doc.DocumentNode
                    .SelectNodes("//h2[contains(text(), 'Sub-committee and Precedent set minutes')]/following-sibling::ul[1]//a")
                    ?.Select(a => new
                    {
                        Url = GetAbsoluteUrl(a.GetAttributeValue("href", "")),
                        Year = a.InnerText.Trim(),
                        Type = "precedent"
                    })
                    .ToList();

                if ((yearLinks == null || !yearLinks.Any()) && (precedentLinks == null || !precedentLinks.Any()))
                {
                    Console.WriteLine("No links found!");
                    return;
                }

                if (yearLinks != null)
                {
                    foreach (var yearLink in yearLinks)
                    {
                        await ProcessYearPage(connection, yearLink.Url, yearLink.Year, yearLink.Type);
                    }
                }

                if (precedentLinks != null)
                {
                    foreach (var precedentLink in precedentLinks)
                    {
                        await ProcessYearPage(connection, precedentLink.Url, precedentLink.Year, precedentLink.Type);
                    }
                }

                transaction.Commit();
            }
            catch
            {
                transaction.Rollback();
                throw;
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error occurred: {ex.Message}");
            throw;
        }
    }

    private async Task ProcessYearPage(SqliteConnection connection, string url, string year, string type)
    {
        Console.WriteLine($"Processing {year}...");

        using var yearCommand = connection.CreateCommand();
        yearCommand.CommandText = @"
            INSERT INTO years (year, url, type)
            VALUES (@year, @url, @type)
            RETURNING id;";
        yearCommand.Parameters.AddWithValue("@year", year);
        yearCommand.Parameters.AddWithValue("@url", url);
        yearCommand.Parameters.AddWithValue("@type", type);
        var yearId = (long)yearCommand.ExecuteScalar();

        var yearPageHtml = await _client.GetStringAsync(url);
        var yearDoc = new HtmlDocument();
        yearDoc.LoadHtml(yearPageHtml);

        var minutesLinks = yearDoc.DocumentNode
            .SelectNodes("//div[contains(@class, 'standard-page-body__content')]//a[contains(@href, '.pdf')]")
            ?.Select(a => new
            {
                Url = GetAbsoluteUrl(a.GetAttributeValue("href", "")),
                Title = a.InnerText.Trim()
            })
            .ToList();

        if (minutesLinks != null && minutesLinks.Any())
        {
            using var minutesCommand = connection.CreateCommand();
            minutesCommand.CommandText = @"
                INSERT INTO minutes (yearId, title, url)
                VALUES (@yearId, @title, @url);";

            foreach (var link in minutesLinks)
            {
                minutesCommand.Parameters.Clear();
                minutesCommand.Parameters.AddWithValue("@yearId", yearId);
                minutesCommand.Parameters.AddWithValue("@title", link.Title);
                minutesCommand.Parameters.AddWithValue("@url", link.Url);
                minutesCommand.ExecuteNonQuery();
            }

            Console.WriteLine($"Found {minutesLinks.Count} minutes links for {year}");
        }
    }
}

public class PdfProcessor : IDisposable
{
    private readonly string _dbPath;
    private readonly HttpClient _client;
    private readonly string _downloadDir;
    private readonly SemaphoreSlim _downloadThrottle = new SemaphoreSlim(3);
    private readonly SemaphoreSlim _dbLock = new SemaphoreSlim(1);
    private SQLiteConnection _db;

    public PdfProcessor(string dbPath, string downloadDir)
    {
        _dbPath = dbPath;
        _downloadDir = downloadDir;
        _client = new HttpClient();
        InitializeDatabase();
    }

    private void InitializeDatabase()
    {
        _db = new SQLiteConnection(_dbPath);
        // Create tables using SQLite.NET ORM (for PDF processing tables)
        _db.CreateTable<ProcessedPdf>();
        _db.CreateTable<CagReference>();
    }

    public void Dispose()
    {
        _db?.Dispose();
        _client?.Dispose();
        _downloadThrottle?.Dispose();
        _dbLock?.Dispose();
    }

    private async Task<T> RetryWithLock<T>(Func<Task<T>> action, int maxRetries = 3)
    {
        for (int i = 0; i < maxRetries; i++)
        {
            try
            {
                await _dbLock.WaitAsync();
                return await action();
            }
            catch (SQLiteException ex) when (ex.Message.Contains("database is locked"))
            {
                if (i == maxRetries - 1) throw;
                await Task.Delay(100 * (i + 1)); // Exponential backoff
            }
            finally
            {
                _dbLock.Release();
            }
        }
        throw new Exception("Should not reach here");
    }

    private async Task RetryWithLockNoResult(Func<Task> action, int maxRetries = 3)
    {
        for (int i = 0; i < maxRetries; i++)
        {
            try
            {
                await _dbLock.WaitAsync();
                await action();
                return;
            }
            catch (SQLiteException ex) when (ex.Message.Contains("database is locked"))
            {
                if (i == maxRetries - 1) throw;
                await Task.Delay(100 * (i + 1)); // Exponential backoff
            }
            finally
            {
                _dbLock.Release();
            }
        }
        throw new Exception("Should not reach here");
    }

    public async Task ProcessAllPdfs()
    {
        using var connection = new SqliteConnection($"Data Source={_dbPath}");
        connection.Open();

        var command = connection.CreateCommand();
        command.CommandText = "SELECT url, title FROM minutes;";

        using var reader = command.ExecuteReader();
        var tasks = new List<Task>();

        while (reader.Read())
        {
            var url = reader.GetString(0);
            var title = reader.GetString(1);
            tasks.Add(ProcessPdfEntry(url, title));
        }

        await Task.WhenAll(tasks);
    }

    private string CalculateFileHash(string filePath)
    {
        using var sha256 = SHA256.Create();
        using var stream = File.OpenRead(filePath);
        var hash = sha256.ComputeHash(stream);
        return Convert.ToBase64String(hash);
    }

    private bool IsFileProcessed(string url, string currentHash)
    {
        var existingFile = _db.Table<ProcessedPdf>()
            .FirstOrDefault(f => f.Url == url);
        return existingFile?.FileHash == currentHash;
    }

    private List<string> GetPageRanges(HashSet<int> pages)
    {
        var ranges = new List<string>();
        var orderedPages = pages.OrderBy(p => p).ToList();

        int start = orderedPages[0];
        int prev = start;

        for (int i = 1; i <= orderedPages.Count; i++)
        {
            if (i == orderedPages.Count || orderedPages[i] != prev + 1)
            {
                ranges.Add(start == prev ? $"p{start}" : $"p{start}-p{prev}");
                if (i < orderedPages.Count)
                {
                    start = orderedPages[i];
                    prev = start;
                }
            }
            else
            {
                prev = orderedPages[i];
            }
        }

        return ranges;
    }

    private async Task<(string FilePath, string Hash)?> DownloadAndHashPdf(string url, string title)
    {
        try
        {
            await _downloadThrottle.WaitAsync();
            var fileName = Path.GetFileName(new Uri(url).LocalPath);
            var filePath = Path.Combine(_downloadDir, fileName);

            if (!File.Exists(filePath))
            {
                Console.WriteLine($"Downloading: {title}");
                var pdfBytes = await _client.GetByteArrayAsync(url);
                await File.WriteAllBytesAsync(filePath, pdfBytes);
            }

            var hash = CalculateFileHash(filePath);
            return (filePath, hash);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error downloading {title}: {ex.Message}");
            return null;
        }
        finally
        {
            _downloadThrottle.Release();
        }
    }

    private Dictionary<string, HashSet<int>> ProcessPdfFile(string filePath)
    {
        var idLocations = new Dictionary<string, HashSet<int>>();
        var pattern = @"\d{2}/CAG/\d{4}";

        using var document = PdfDocument.Open(filePath);
        foreach (var page in document.GetPages())
        {
            var pageNumber = page.Number;
            var matches = Regex.Matches(page.Text, pattern);

            foreach (Match match in matches)
            {
                var id = match.Value;
                if (!idLocations.ContainsKey(id))
                {
                    idLocations[id] = new HashSet<int>();
                }
                idLocations[id].Add(pageNumber);
            }
        }

        return idLocations;
    }

    private async Task ProcessPdfEntry(string url, string title)
    {
        try
        {
            // Check if already processed
            var processed = await RetryWithLock(async () =>
                _db.Table<ProcessedPdf>()
                    .FirstOrDefault(p => p.Url == url));

            if (processed != null)
            {
                Console.WriteLine($"Already processed: {title}");
                return;
            }

            // Download and hash the PDF
            var result = await DownloadAndHashPdf(url, title);
            if (result == null) return;

            var (filePath, hash) = result.Value;

            // Process the PDF for CAG references
            var idLocations = ProcessPdfFile(filePath);

            // Save results to database
            await RetryWithLockNoResult(async () =>
            {
                _db.RunInTransaction(() =>
                {
                    // Update processed file record
                    _db.InsertOrReplace(new ProcessedPdf
                    {
                        Url = url,
                        FileHash = hash,
                        ProcessedDate = DateTime.UtcNow
                    });

                    // Remove existing CAG ID locations for this file
                    _db.Execute("DELETE FROM CagReferences WHERE PdfUrl = ?", url);

                    // Insert new locations
                    foreach (var entry in idLocations)
                    {
                        var pageRanges = string.Join(", ", GetPageRanges(entry.Value));
                        _db.Insert(new CagReference
                        {
                            PdfUrl = url,
                            CagId = entry.Key,
                            PageRanges = pageRanges,
                            ProcessedDate = DateTime.UtcNow
                        });
                    }
                });
                await Task.CompletedTask;
            });

            Console.WriteLine($"Found {idLocations.Count} CAG references in {title}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error processing {title}: {ex.Message}");
        }
    }
}
