package com.acm.api_emdha.serviceImpl;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.List;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.acm.api_emdha.service.EmdhaApiService;
import com.acm.client.CreditClient;
import com.acm.client.GedClient;
import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonConstantGED;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.acm.utils.validation.ACMValidationUtils;
import com.emdha.esign.DummyHostnameVerifier;
import com.emdha.esign.DummyTrustManager;
import com.emdha.esign.EmdhaInput;
import com.emdha.esign.EmdhaServiceReturn;
import com.emdha.esign.EmdhaSignerInfo;
import com.emdha.esign.ReturnDocument;
import com.emdha.esign.eSignEmdha;
import com.google.common.base.Preconditions;

import esign.text.pdf.codec.Base64;

/**
 * The Class EmdhaApiServiceImpl.
 */
@Service
public class EmdhaApiServiceImpl implements EmdhaApiService {

	/** The ged client. */
	@Autowired
	private GedClient gedClient;

	/** The ged client. */
	@Autowired
	private CreditClient creditClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The temp folder path. */
	private String tempFolderPath = "";

	/** The licence file path. */
	private String licenceFilePath = "";

	/** The p FX file path. */
	private String pFXFilePath = "";

	/** The p FX password. */
	private String pFXPassword = "emdha";

	/** The p FX alias. */
	private String pFXAlias = "fin";

	/** The p SIPID. */
	private String pSIPID = "";

	/** The e sign URL. */
	private String eSignURL = "";

	/** The emdha organization. */
	private String emdhaOrganization = "Org";

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(EmdhaApiServiceImpl.class);

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#emdhaApi(com.acm.utils.dtos.LoanDTO, java.lang.String)
	 */
	@Override
	public boolean emdhaApi(LoanDTO loanDTO) {

		AcmDocumentsDTO acmDocumentsDTO = new AcmDocumentsDTO();
		// category agreement
		acmDocumentsDTO.setLoanId(loanDTO.getLoanId());
		logger.info("loan id to find documents for emdha api : {}", loanDTO.getLoanId());
		SettingDocumentTypeDTO docTypeDto = new SettingDocumentTypeDTO(2);
		acmDocumentsDTO.setSettingDocumentTypeDTO(docTypeDto);
		logger.info("acmDocumentsDTO for emdha api : {}", acmDocumentsDTO);
		List<AcmDocumentsDTO> listDocuments = creditClient.find(acmDocumentsDTO);
		logger.info("Documents to sign for emdha api : {}", listDocuments);
		logger.info("size listDocuments to sign for emdha api : {}", listDocuments.size());

		List<byte[]> listByteDocuments = new ArrayList<>();

		for (AcmDocumentsDTO acmDocumentDTO : listDocuments) {

			byte[] documentGed = null;
			if (ACMValidationUtils.isNullOrEmpty(acmDocumentDTO.getIdDocumentGED())) {
				documentGed = gedClient.findPhotoClient(acmDocumentDTO.getIdDocument());
				logger.info("Documents GED from find photo for emdha api : {}", documentGed);

			}
			else {
				documentGed = gedClient.displayDocument(acmDocumentDTO.getIdDocumentGED());
				logger.info("Documents GED from display document  for emdha api : {}", documentGed);
			}
			if (!ACMValidationUtils.isNullOrEmpty(documentGed)) {
				listByteDocuments.add(documentGed);
				logger.info("list Byte Documents  for emdha api : {}", listByteDocuments);
			}
		}

		return treatmentReportsEmdha(listByteDocuments, listDocuments, loanDTO);

	}

	/**
	 * Check file exists.
	 *
	 * @param filePath the file path
	 * @return true, if successful
	 */
	private static boolean checkFileExists(String filePath) {

		Path path = Paths.get(filePath);
		return Files.exists(path);
	}

	/**
	 * Treatment reports emdha.
	 *
	 * @param listPdfBytes the list pdf bytes
	 * @param listDocuments the list documents
	 * @param loanDto the loan dto
	 * @return true, if successful
	 */
	// private boolean treatmentReportsEmdha(List<byte[]> pdfPath,
	private boolean treatmentReportsEmdha(List<byte[]> listPdfBytes,
			List<AcmDocumentsDTO> listDocuments, LoanDTO loanDto) {

		try {

			List<AcmEnvironnementDTO> environnementDTOs =
					parametrageClient.findLikeKey(new AcmEnvironnementDTO("EMDHA_API_REQUEST"));

			tempFolderPath = CommonFunctions.findConfigurationFromList(environnementDTOs,
					"EMDHA_API_REQUEST_TEMPDIR", tempFolderPath);
			// "D:/Sanad/M1_SDK_BTSHL_Java/M1_SDK_BTSHL_Java/Sample/";

			// In output folder path, signed PDFs are written.
			String outputFolder = tempFolderPath + "Output";

			// Licence File
			// String licenceFilePath = "UAT-BFSI-BTSHL01.lic";
			// String licenceFilePath = tempFolderPath + "UAT-BFSI-BTSHL01.lic";

			licenceFilePath = CommonFunctions.findConfigurationFromList(environnementDTOs,
					"EMDHA_API_REQUEST_LICENCEFILE", licenceFilePath);
			System.out.println("Path to UAT-BFSI-BTSHL01.lic: " + licenceFilePath);
			logger.info("Path to UAT-BFSI-BTSHL01.lic:: {}", licenceFilePath);
			pFXFilePath = CommonFunctions.findConfigurationFromList(environnementDTOs,
					"EMDHA_API_REQUEST_PFXFILEPATH", pFXFilePath);

			// Use the file paths as needed
			System.out.println("Path to pFXFilePath: " + pFXFilePath);
			logger.info("Path to pFXFilePath:: {}", pFXFilePath);

			// SIP Certificate
			// String pFXFilePath = tempFolderPath + "ClientSDKDev.pfx";
			// String pFXPassword = "emdha";
			// String pFXAlias = "fin";

			pFXPassword = CommonFunctions.findConfigurationFromList(environnementDTOs,
					"EMDHA_API_REQUEST_PFXPASSWORD", pFXPassword);
			pFXAlias = CommonFunctions.findConfigurationFromList(environnementDTOs,
					"EMDHA_API_REQUEST_PFXALIAS", pFXAlias);

			// SIP ID
			// String pSIPID = "UAT-BFSI-BTSHL01";
			pSIPID = CommonFunctions.findConfigurationFromList(environnementDTOs,
					"EMDHA_API_REQUEST_SIPID", pSIPID);

			// eSign URL
			// String eSignURL = "https://esign-dev.emdha.sa/eSign/SignDoc";
			eSignURL = CommonFunctions.findConfigurationFromList(environnementDTOs,
					"EMDHA_API_REQUEST_ESIGNURL", eSignURL);

			// check existance files
			String[] filePaths = {licenceFilePath, pFXFilePath};

			for (String filePath : filePaths) {
				if (checkFileExists(filePath)) {
					logger.info("exists at the specified path:  {}  ", filePath);

				}
				else {
					logger.info("does not exist at the specified path:  {}  ", filePath);
				}
			}

			// KYC ID
			CustomerDTO customer = loanDto.getCustomerDTO();
			String kycId = customer.getIdentity();
			logger.info("national id of customer for emdha api  ", kycId);

			List<String> listDocBase64 = docBase64FromByte(listPdfBytes);

			logger.info("listDocBase64 for emdha api : {}", listDocBase64);
			// Step-1.Convert PDF to Base64 encoded string
			// String pdfPath = tempFolderPath + "Contract.pdf";
			// String docBase64 = docBase64(pdfPath);

			// System.out.println("docBase64 >>>\n"+docBase64+"\n<<<<");
			// String appearanceBackgroundImage =
			// "C:/Users/mkhemissi/Downloads/M1_SDK_BTSHL_Java
			// (1)/M1_SDK_BTSHL_Java/License/talys.png";
			String appearanceBackgroundImage = "";

			// Step-2. Construct eSignEmdha object using licenceFile, PFXFile, PFXFile credentials
			// and SIPID. (PFX file is also called certificate file.)
			// These files are available in the integration kit shared with you. Your SIPID is
			// UAT-BFSI-RAJHI01
			// eSignEmdha eSign = new eSignEmdha(licenceFilePath, //pFXFilePath, pFXPassword,
			// pFXAlias, pSIPID); // by default signature contentEstimated is 40000
			eSignEmdha eSign = new eSignEmdha(licenceFilePath, pFXFilePath, pFXPassword, pFXAlias,
					pSIPID, 40000); // 6th parameter is signature contentEstimated

			// Step-3. Construct EmdhaSignerInfo object, the value to the parameters of this
			// constructor is supposed to be obtained from Trusted KYC source with 2 factor
			// authentication.

			emdhaOrganization = CommonFunctions.findConfigurationFromList(environnementDTOs,
					"EMDHA_API_REQUEST_ORGANIZATION", emdhaOrganization);

			String customerNameNoPipe = customer.getCustomerName().replaceAll("\\|", " ");

			EmdhaSignerInfo signerInfo = new EmdhaSignerInfo(emdhaOrganization, kycId,
					customerNameNoPipe, customerNameNoPipe, customer.getTelephone(),
					customer.getEmail(), "address", "RIYADH", "SA");

			logger.info("signerInfo: {}", signerInfo.toString());

			String signerInfox64 = signerInfo.getSignerInfoXMLBase64();
			logger.info("signerInfox64  for emdha api : {}", signerInfox64);

			// signerInfo.getSignerInfoXML();

			// Step-4. Create EmdhaInput object.

			// for adding custom image/logo to the signature appearance.
			// appearanceBackgroundImage is the image of your organization logo. Please provide
			// base64 of the logo.
			// Please choose eSignEmdha.SignatureAppearanceType.CUSTOM_LOGO in EmdhaInput
			// constructor.
			// If eSignEmdha.SignatureAppearanceType.EMDHA_LOGO chosen then emdha logo appears.
			// //default
			// If eSignEmdha.SignatureAppearanceType.NO_IMAGE chosen then no logo appears
			// appearanceBackgroundImage = docBase64("your organization logo.png"); //uncomment to
			// use this

			// EmdhaInput input1 = new EmdhaInput(docBase64, "Saurabh", true,
			// eSignEmdha.PageTobeSigned.All, eSignEmdha.Coordinates.CentreMiddle,
			// eSignEmdha.AppearanceRunDirection.RUN_DIRECTION_LTR,
			// eSignEmdha.SignatureAppearanceType.EMDHA_LOGO, appearanceBackgroundImage,
			// "Custom content");

			ArrayList<EmdhaInput> inputs = new ArrayList<>();

			// Preapre a list of documents to be signed. Maximum 10 documents can be signed in a
			// single transaction.
			for (String docBase64 : listDocBase64) {
				// EmdhaInput, some other constructor samples
				// inputs.add(new EmdhaInput(docBase64, "", true,
				// "1-94,575,244,650;2-75,695,225,770",
				inputs.add(new EmdhaInput(docBase64, "", true, eSignEmdha.PageTobeSigned.All,
						eSignEmdha.Coordinates.BottomRight,
						eSignEmdha.AppearanceRunDirection.RUN_DIRECTION_RTL,
						eSignEmdha.SignatureAppearanceType.EMDHA_LOGO, appearanceBackgroundImage,
						"Signed By: " + customerNameNoPipe));

			}

			logger.info("e sign inputs: {}", inputs);

			// Step-5. Generate request XML to post it to EMDHA.
			// second parameter here is transaction ID. Transaction ID should be unique for each
			// transaction. If passed empty SDK generates a transaction ID.
			EmdhaServiceReturn serviceReturn = eSign.generateRequestXml(inputs, "", "",
					signerInfox64, kycId, true, true, eSignEmdha.KYCIdProvider.SELF_NID);

			logger.info("Service return for emdha api. Inputs: {}, Signer: {}, KYC ID: {}", inputs,
					signerInfox64, kycId);

			logger.info("Service return status for emdha api: {}", serviceReturn.getStatus());
			logger.info("Service return getErrorMessage for emdha api: {}",
					serviceReturn.getErrorMessage());
			logger.info("Service return getErrorCode for emdha api: {}",
					serviceReturn.getErrorCode());
			logger.info("Service return getRequestXML for emdha api: {}",
					serviceReturn.getRequestXML());

			ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();

			thirdPartyHistoDTO.setCategory("EMDHA");
			thirdPartyHistoDTO.setRequestValue(inputs.toString());
			thirdPartyHistoDTO.setResponseValue(String.valueOf(serviceReturn.getReturnValues()));
			thirdPartyHistoDTO.setStatus(String.valueOf(serviceReturn.getStatus()));
			creditClient.create(thirdPartyHistoDTO);
			if (serviceReturn.getStatus() != 1) {

				return false;
			}
			else {
				// URL encode the generated request XML.
				String URLEncodedsignedRequestXML =
						URLEncoder.encode(serviceReturn.getRequestXML(), "UTF-8");

				System.err.println(
						"\n>>>URL Encoded signed RequestXML \n" + URLEncodedsignedRequestXML);

				logger.info("eSignURL emdha api  ", eSignURL, "; " + URLEncodedsignedRequestXML);

				// Step-6. Post request XML to emdha CA and get response XML for signing completion
				String response = postXML(eSignURL, URLEncodedsignedRequestXML);

				logger.info("eSignURL: {}", eSignURL);

				thirdPartyHistoDTO.setCategory("EMDHA");
				thirdPartyHistoDTO
						.setRequestValue(eSignURL + "url encoded: " + URLEncodedsignedRequestXML);
				thirdPartyHistoDTO.setResponseValue("response: " + response);
				thirdPartyHistoDTO.setStatus(String.valueOf(serviceReturn.getStatus()));
				creditClient.create(thirdPartyHistoDTO);

				logger.info("Response for emdha API: {}", response);

				System.err.println("\n>>>response request XML \n" + response);

				// Step-7. call getSignedDocuments method
				EmdhaServiceReturn eSignServiceReturn =
						eSign.getSignedDocuments(response, serviceReturn.getReturnValues());

				System.err.println("eSign Service Return " + eSignServiceReturn);
				logger.info("e sign service return: {}", eSignServiceReturn);

				// EmdhaServiceReturn eSignServiceReturn = eSign.getSignedDocuments(response,
				// tempFolderPath); //temp file path is required. ".Sig" file is created for the
				// transaction and written it to the tempFilePath.
				logger.info("eSignServiceReturn status for emdha API: {}",
						eSignServiceReturn.getStatus());

				if (eSignServiceReturn.getStatus() == 1) {
					int index = 0;

					List<MultipartFile> multipartFiles = new ArrayList<>();
					// call getReturnValues method to get signed PDFs
					ArrayList<ReturnDocument> docs = eSignServiceReturn.getReturnValues();

					logger.info("docs for emdha api  ", docs);

					System.err.println("docs " + docs);
					// List<MultipartFile[]> listMultiPartFile = new ArrayList<>();
					for (ReturnDocument doc : docs) {
						// System.err.println("doc " + doc);
						String pdfToBase64 = doc.getSignedDocument();
						System.out.println("pdfToBase64 >>>\n" + pdfToBase64 + "\n<<<<");
						byte[] signedBytes = Base64.decode(pdfToBase64);
						String pdfOUT = "";

						// Write signed PDF to output folder path --- > to be removed later
						pdfOUT = outputFolder + File.separator + "Signed_PDF_" + index + ".pdf";
						try (FileOutputStream fos = new FileOutputStream(pdfOUT)) {
							fos.write(signedBytes);
						}
						catch (Exception e) {
							System.out.println(e);
						}

						// set documet acm with size
						listDocuments.get(index).setDocumentSize(signedBytes.length);

						MultipartFile[] multiparFile =
								CommonFunctions.convertToMultipartFiles(signedBytes);

						System.err.println("multiparFile " + multiparFile[0]);

						listDocuments.get(index).setMultipartFile(multiparFile[0]);
						logger.info(
								"Document when we set multipartfile for emdha API. Response: {}",
								listDocuments.get(index));
						multipartFiles.add(multiparFile[0]);

						logger.info(
								"multipartFiles when we set multipartfile for emdha API. Response: {}",
								multipartFiles);

						index++;

					}

					saveToGed(multipartFiles.toArray(new MultipartFile[listDocuments.size()]),
							listDocuments, loanDto);
					logger.info("Saved data to GED for emdha API. Response: {}", response);

					return true;
				}
			}
		}
		catch (Exception ex) {
			logger.info("exception for emdha api: {}", ex.getMessage());

			logger.info("exception for emdha api  ", ex);

			System.out.println(ex);
		}
		return true;

	}

	/**
	 * Post XML.
	 *
	 * @param eSignURL the e sign URL
	 * @param requestXML the request XML
	 * @return the string
	 * @throws Exception the exception
	 */
	// Method to post request XML
	private static String postXML(String eSignURL, String requestXML) throws Exception {

		URL url = new URL(eSignURL);
		HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();

		SSLContext sslcontext = SSLContext.getInstance("TLSv1.2");
		sslcontext.init(new KeyManager[0], new TrustManager[] {new DummyTrustManager()},
				new SecureRandom());
		SSLSocketFactory factory = sslcontext.getSocketFactory();

		connection.setRequestMethod("POST");
		connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
		connection.setRequestProperty("Content-Length",
				"" + Integer.toString(requestXML.getBytes().length));
		connection.setRequestProperty("Content-Language", "en-US");
		connection.setUseCaches(false);
		connection.setDoInput(true);
		connection.setDoOutput(true);
		connection.setSSLSocketFactory(factory);
		connection.setHostnameVerifier(new DummyHostnameVerifier());

		try (DataOutputStream wr = new DataOutputStream(connection.getOutputStream())) {
			wr.writeBytes(requestXML);
			wr.flush();
		}
		InputStream is = connection.getInputStream();
		String response;
		try (BufferedReader rd = new BufferedReader(new InputStreamReader(is))) {
			String line = null;
			response = "";
			while ((line = rd.readLine()) != null) {
				response = response + line + "\r";
			}
		}
		return response;
	}

	/**
	 * Doc base 64.
	 *
	 * @param listSamplePDF the list sample PDF
	 * @return the list
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private static List<String> docBase64FromByte(List<byte[]> listSamplePDF) throws IOException {

		List<String> listResultEncode = new ArrayList<>();

		for (byte[] samplePDF : listSamplePDF) {
			listResultEncode.add(Base64.encodeBytes(samplePDF));
		}
		return listResultEncode;
	}

	/**
	 * Doc base 64.
	 *
	 * @param filePath the file path
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private static String docBase64(String filePath) throws IOException {

		File file = new File(filePath);
		InputStream inputStream = new FileInputStream(file);
		byte[] samplePDF = new byte[(int) file.length()];
		inputStream.read(samplePDF);
		inputStream.close();
		return Base64.encodeBytes(samplePDF);
	}

	/**
	 * Convert multipart file to byte array.
	 *
	 * @param file the file
	 * @return the byte[]
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public static byte[] convertMultipartFileToByteArray(MultipartFile file) throws IOException {

		return file.getBytes();
	}

	/**
	 * Save to ged.
	 *
	 * @param uploadedFiles the uploaded files
	 * @param documentsWithmultipartFile the documents withmultipart file
	 * @param loanDTO the loan DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */

	private List<AcmDocumentsDTO> saveToGed(MultipartFile[] uploadedFiles,
			List<AcmDocumentsDTO> documentsWithmultipartFile, LoanDTO loanDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(documentsWithmultipartFile,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(uploadedFiles, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// processing data into ACM
		List<AcmDocumentsDTO> newDocumentsWithmultipartFile = new ArrayList<>();
		List<AcmDocumentsDTO> disbledAcmDocumentsDTOs = new ArrayList<>();
		List<GedParameterDTO> gedParameterDTOs = new ArrayList<>();
		logger.info("documents With multipartFile for emdha API. Response: {}",
				documentsWithmultipartFile);

		for (AcmDocumentsDTO acmDocumentsDTO : documentsWithmultipartFile) {
			// find list of duplicated document by titre if exist idLoan not null
			disbledAcmDocumentsDTOs.add(new AcmDocumentsDTO(acmDocumentsDTO.getIdDocument()));
			logger.info("disbled AcmDocumentsDTOs for emdha API. Response: {}",
					disbledAcmDocumentsDTOs);

			logger.info("tempFolderPath for emdha API. Response: {}", tempFolderPath);
			// convert MultipartFile to File
			File file = CommonFunctions.fileConverter(acmDocumentsDTO.getMultipartFile(),
					tempFolderPath);

			logger.info("size of file for emdha API. Response: {}", file.length());
			acmDocumentsDTO.setDateCreation(null);
			acmDocumentsDTO.setIdDocument(null);
			acmDocumentsDTO.setMultipartFile(null);
			logger.info("acmDocumentsDTO for emdha API. Response: {}", acmDocumentsDTO);

			// save data document loan in db
			AcmDocumentsDTO newAcmDocumentsDTO = creditClient.createAcmDocuments(acmDocumentsDTO);
			logger.info("newAcmDocumentsDTO for emdha API. Response: {}", newAcmDocumentsDTO);

			// init data to be send to GED
			GedParameterDTO gedParameterDTO = new GedParameterDTO();
			gedParameterDTO.setPath(CommonConstants.APP_CLIENT);
			gedParameterDTO.setSite(CommonConstantGED.ACM_SITE);
			gedParameterDTO.setAcmDocumentsDTO(newAcmDocumentsDTO);
			// setting tags
			List<String> tags = new ArrayList<>();
			tags.add(newAcmDocumentsDTO.getTitre());
			if (!ACMValidationUtils.isNullOrEmpty(newAcmDocumentsDTO.getLoanId())) {
				// find data of loan
				tags.add(loanDTO.getAccountNumber());
				tags.add(loanDTO.getProductCode());
				// set loan number
				gedParameterDTO.setLoanNumber(loanDTO.getAccountNumber());
				// set customer number
				gedParameterDTO.setCustomerNumber(loanDTO.getCustomerDTO().getCustomerNumber());
				// setting data for backup if there is an exception
				gedParameterDTO.setIdCustomer(loanDTO.getCustomerDTO().getId());
				gedParameterDTO.setIdDocument(newAcmDocumentsDTO.getIdDocument());
				gedParameterDTO.setLoanId(loanDTO.getLoanId());
			}

			gedParameterDTO.setTags(tags);
			logger.info("acmDocumentsDTO22 for emdha API. Response: {}", acmDocumentsDTO);

			List<File> filesToSend = new ArrayList<>();
			filesToSend.add(file);
			logger.info("file for emdha API. Response: {}", file);
			logger.info("filesToSend for emdha API. Response: {}", filesToSend);
			gedParameterDTO.setFilesToUpload(filesToSend);
			logger.info("gedParameterDTO for emdha API. Response: {}", gedParameterDTO.toString());

			gedParameterDTOs.add(gedParameterDTO);
		}
		// send All document to GED
		try {
			// send to GED
			logger.info("gedParameterDTOs for emdha API. Response: {}", gedParameterDTOs);
			List<GedParameterDTO> ids = gedClient.uploadListToGed(gedParameterDTOs);
			logger.info("ids for emdha API. Response: {}", ids);
			for (GedParameterDTO gedParameterDTO : ids) {
				logger.info("gedParameterDTO for emdha API. Response: {}",
						gedParameterDTO.toString());
				String idDocumentGED =
						ACMValidationUtils.isNullOrEmpty(gedParameterDTO.getIdDocumentGed()) ? null
								: gedParameterDTO.getIdDocumentGed().split(";")[0];
				logger.info("idDocumentGed inserted : {} ", idDocumentGED);

				// update data : id DocumentGED if is not NULL
				AcmDocumentsDTO acmDocumentsDTO = gedParameterDTO.getAcmDocumentsDTO();
				logger.info("acmDocumentsDTO333 for emdha API. Response: {}", acmDocumentsDTO);
				if (!ACMValidationUtils.isNullOrEmpty(idDocumentGED)) {
					acmDocumentsDTO.setIdDocumentGED(idDocumentGED);
					newDocumentsWithmultipartFile
							.add(creditClient.updateAcmDocuments(acmDocumentsDTO));

					logger.info("newDocumentsWithmultipartFile1 for emdha API. Response: {}",
							newDocumentsWithmultipartFile);
				}
				else { // if GED id down
					newDocumentsWithmultipartFile.add(acmDocumentsDTO);
					logger.info("newDocumentsWithmultipartFile2 for emdha API. Response: {}",
							newDocumentsWithmultipartFile);

				}

				for (AcmDocumentsDTO acmDocumentsDto : disbledAcmDocumentsDTOs) {
					logger.info("acmDocumentsDto44 for emdha API. Response: {}", acmDocumentsDto);
					creditClient.disableDocument(acmDocumentsDto);
					logger.info("disable document4444 for emdha API. Response: {}",
							acmDocumentsDto);
				}

			}
		}
		catch (Exception e) {
			logger.error("failed to save in GED");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// delete inserted Document in ACM
			for (GedParameterDTO gedParameterDTO : gedParameterDTOs) {
				logger.info("gedParameterDTOs for emdha API. Response: {}", gedParameterDTOs);
				creditClient.deleteDocument(gedParameterDTO.getIdDocument());
			}
		}

		logger.info("result newDocumentsWithmultipartFile for emdha API. Response: {}",
				newDocumentsWithmultipartFile);
		// retrun data
		return newDocumentsWithmultipartFile;

	}

}
