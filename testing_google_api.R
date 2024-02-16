library(googlesheets4)


gs4_auth_configure(path = "C:/Users/skt/Documents/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")


brizo_us_zh <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Brizo - US (Chinese 1-41968)")
brizo_us_eng <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Brizo - US (English 49079-56183)")
brizo_can <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Brizo - CAN (Chinese 41969-49078)")
printshop_us <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Print Shop - US (Chinese 56184-80086)")
printshop_can <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Print Shop - CAN (80087-80422)")
oldlist_can <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Old List - CAN (96270-101360)")
oldlist_us <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Old List - US (80423-96269)")
