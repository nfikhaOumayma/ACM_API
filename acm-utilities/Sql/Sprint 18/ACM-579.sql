----- TABLE : [ACM_SETTING_MOTIFS_REJET]

INSERT INTO [dbo].[ACM_SETTING_MOTIFS_REJET]  ([CATEGORIE],[CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES ('SUBJECT_IB','INCIDENT',N'Incident / بلاغ',N'قبل اتخاذ اجراء قانوني ضد العميل',1,GETDATE(),'ADMIN',0),
			('SUBJECT_IB','ENQUERY',N'Enquery / استعلام',N'عام - لمنتج اوخدمة',1,GETDATE(),'ADMIN',0),
			('SUBJECT_IB','SUGGESTION',N'Suggestion / اقتراح',N'ممكن ان يزودنا العميل باقتراح لتحسين المنتج او اخذ تمويل إضافي',1,GETDATE(),'ADMIN',0),
			('SUBJECT_IB','LOAN_RENEW_REQUEST',N'Loan renew Request (for working loans) / تجديد قرض',N'للعملاء الجيدين',1,GETDATE(),'ADMIN',0),
			('SUBJECT_IB','EVALUATION',N'Evaluation / تقييم الخدمة',N'بعد التمويل',1,GETDATE(),'ADMIN',0),
			('SUBJECT_IB','MEETING_REQUEST',N'Meeting request / حجز اجتماع',N'اجتماع اللجنة',1,GETDATE(),'ADMIN',0),
			('SUBJECT_IB','COMPLAINT',N'Complaint / شكوى',N'ارسال رسائل بقنوات الشكاوى المتاحة للعملاء',1,GETDATE(),'ADMIN',0),
			('SUBJECT_IB','OTHER',N'Other / طلب آخر',N'طلب آخر',1,GETDATE(),'ADMIN',0),
			('SUBJECT_IB','CANCEL_APPlICATION',N'Cancel application / إلغاء التطبيق',N'إلغاء التطبيق',1,GETDATE(),'ADMIN',0);

--------- ALTER TABLE : ACM_CUSTOMER_CONTACT
--------- ADD COLUMN : SENT_CUSTOMER

ALTER TABLE ACM_CUSTOMER_CONTACT ADD SENT_CUSTOMER bit;


--------- ALTER TABLE : ACM_CUSTOMER_CONTACT
--------- ADD COLUMN : USERNAME


ALTER TABLE ACM_CUSTOMER_CONTACT ADD USERNAME varchar(256);

