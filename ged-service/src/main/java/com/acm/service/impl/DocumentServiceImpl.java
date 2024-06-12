/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.CmisObject;
import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.enums.VersioningState;
import org.apache.chemistry.opencmis.commons.exceptions.CmisConnectionException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisContentAlreadyExistsException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ContentStreamImpl;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;

import com.acm.config.ged.GedConfigProperties;
import com.acm.constants.common.CommonConstantGED;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.GEDException;
import com.acm.ged.factory.GedServiceFactory;
import com.acm.service.AcmDocumentsGedService;
import com.acm.service.DocumentService;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.AcmDocumentsGedDTO;
import com.acm.utils.dtos.GedDocumentDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link DocumentServiceImpl} class.Services relatives to GED manipulation.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
@Service
public class DocumentServiceImpl implements DocumentService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(DocumentServiceImpl.class);

	/** The ged service factory. */
	@Autowired
	private GedServiceFactory gedServiceFactory;

	/** The acm documents ged service. */
	@Autowired
	private AcmDocumentsGedService acmDocumentsGedService;

	/** The Environment. */
	@Autowired
	private Environment environment;
	@Autowired
	GedConfigProperties gedConfigProperties;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#fileUploadToGed(org.springframework.web.multipart.
	 * MultipartFile[], java.lang.String)
	 */
	@Override
	public List<String> uploadToGed(GedParameterDTO fileParameters)
			throws GEDException.FileExisting, FileNotFoundException {

		Preconditions.checkNotNull(fileParameters, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		List<String> returnedDocuments = new ArrayList<>();
		Boolean exceptionOccured = Boolean.FALSE;
		try {
			// Build session
			Session session = null;
			try {
				session = gedServiceFactory.createSession();
			}
			catch (Exception e) {
				logger.error("CmisConnectionException : {}", e.getMessage());
				// Add document in ACM DB for backup
				// save file in ACM as Backup
				saveFilesInACM(fileParameters.getFilesToUpload().get(0),
						fileParameters.getIdDocument(), fileParameters.getLoanId(),
						fileParameters.getIdCustomer(), fileParameters.getAcmDocumentsDTO());
				return new ArrayList<>();
			}
			// INIT Destination Folder
			Folder destinationFolder = initDestinationFolder(session, fileParameters);
			logger.info("*** NB of files to be uploaded = {} ***",
					fileParameters.getFilesToUpload().size());
			// processing given data
			for (File file : fileParameters.getFilesToUpload()) {
				try {

					Document newDocument =
							upload(file, destinationFolder, fileParameters.getTags(), session);
					logger.info("*** fileParameters to upload = {} ***",
							fileParameters.getFilesToUpload().size());
					logger.info("*** session = {} ***", session);
					logger.info("*** file = {} ***", file);

					logger.info("*** newDocument to upload = {} ***", newDocument.toString());
					if (newDocument != null) {
						returnedDocuments.add(newDocument.getId());
					}
				}
				catch (CmisContentAlreadyExistsException exception) {
					String message = "Exception has been occurred while uploading file.";
					logger.error(message, exception);
					ExceptionResponseMessage exceptionResponseMessage =
							new ExceptionResponseMessage(CommonErrorCode.UPLOAD_FAILED, message,
									new TechnicalException(
											CommonExceptionsMessage.EXCEPTIONS_SITE_NOT_FOUND));
					logger.error("{}", exceptionResponseMessage);
					// Add document in ACM DB for backup
					saveFilesInACM(file, fileParameters.getIdDocument(), fileParameters.getLoanId(),
							fileParameters.getIdCustomer(), fileParameters.getAcmDocumentsDTO());
				}
			}
			logger.info("uploaded docs in GED :: DONE");
		}
		catch (CmisConnectionException ex) {
			exceptionOccured = Boolean.TRUE;
			logger.error("Connection Failed to GED.", ex);
		}
		catch (CmisObjectNotFoundException ex) {
			exceptionOccured = Boolean.TRUE;
			logger.error("The path to save the artifact to ('{}') is not found.",
					fileParameters.getPath(), ex);
		}
		catch (Exception ex) {
			exceptionOccured = Boolean.TRUE;
			logger.error("Exception has been occurred  while uploading file : ", ex);
		}

		// Processing file if an exception has been occurred
		if (Boolean.TRUE.equals(exceptionOccured)) {
			// Add document in ACM DB for backup
			for (File file : fileParameters.getFilesToUpload()) {
				// save file in ACM as Backup
				saveFilesInACM(file, fileParameters.getIdDocument(), fileParameters.getLoanId(),
						fileParameters.getIdCustomer(), fileParameters.getAcmDocumentsDTO());
			}
		}
		return returnedDocuments;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#uploadListToGed(java.util.List)
	 */
	@Override
	public List<GedParameterDTO> uploadListToGed(List<GedParameterDTO> gedParameterDTOs)
			throws GEDException, FileNotFoundException {

		Preconditions.checkNotNull(gedParameterDTOs,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("*** NB of files to be uploaded = {} ***", gedParameterDTOs.size());
		// Build session
		Session session = null;
		try {
			session = gedServiceFactory.createSession();
		}
		catch (Exception e) {
			logger.error("CmisConnectionException : {}", e.getMessage());
			for (GedParameterDTO fileParameters : gedParameterDTOs) {
				// Add document in ACM DB for backup
				// save file in ACM as Backup
				saveFilesInACM(fileParameters.getFilesToUpload().get(0),
						fileParameters.getIdDocument(), fileParameters.getLoanId(),
						fileParameters.getIdCustomer(), fileParameters.getAcmDocumentsDTO());
			}
			return gedParameterDTOs;
		}
		Folder destinationFolder = null;
		List<GedParameterDTO> returnedDocuments = new ArrayList<>();
		Boolean exceptionOccured = Boolean.FALSE;
		for (GedParameterDTO fileParameters : gedParameterDTOs) {
			try {
				// INIT Destination Folder
				if (destinationFolder == null) {
					destinationFolder = initDestinationFolder(session, fileParameters);
				}
				logger.info("Destination Folder= {}", destinationFolder);
				// processing given data
				try {
					logger.info("FILE NAME= {}",
							fileParameters.getFilesToUpload().get(0).getName());
					Document newDocument = upload(fileParameters.getFilesToUpload().get(0),
							destinationFolder, fileParameters.getTags(), session);
					if (newDocument != null) {
						fileParameters.setIdDocumentGed(newDocument.getId());
						returnedDocuments.add(fileParameters);
					}
				}
				catch (CmisContentAlreadyExistsException exception) {
					String message = "Exception has been occurred while uploading file.";
					logger.error(message, exception);
					ExceptionResponseMessage exceptionResponseMessage =
							new ExceptionResponseMessage(CommonErrorCode.UPLOAD_FAILED, message,
									new TechnicalException(
											CommonExceptionsMessage.EXCEPTIONS_SITE_NOT_FOUND));
					logger.error("{}", exceptionResponseMessage);
					// Add document in ACM DB for backup
					saveFilesInACM(fileParameters.getFilesToUpload().get(0),
							fileParameters.getIdDocument(), fileParameters.getLoanId(),
							fileParameters.getIdCustomer(), fileParameters.getAcmDocumentsDTO());
				}
				logger.info("uploaded docs in GED :: DONE");
			}
			catch (CmisConnectionException ex) {
				exceptionOccured = Boolean.TRUE;
				logger.error("Connection Failed to GED.", ex);
			}
			catch (CmisObjectNotFoundException ex) {
				exceptionOccured = Boolean.TRUE;
				logger.error("The path to save the artifact to ('{}') is not found.",
						fileParameters.getPath(), ex);
			}
			catch (Exception ex) {
				exceptionOccured = Boolean.TRUE;
				logger.error("Exception has been occurred  while uploading file : ", ex);
			}

			// Processing file if an exception has been occurred
			if (Boolean.TRUE.equals(exceptionOccured)) {
				// Add document in ACM DB for backup
				// save file in ACM as Backup
				saveFilesInACM(fileParameters.getFilesToUpload().get(0),
						fileParameters.getIdDocument(), fileParameters.getLoanId(),
						fileParameters.getIdCustomer(), fileParameters.getAcmDocumentsDTO());
			}
		}
		// returning data
		return returnedDocuments;
	}

	/**
	 * Inits the destination folder.
	 * 
	 * @author HaythemBenizid
	 * @param session the session
	 * @param fileParameters the file parameters
	 * @return the folder
	 */
	private Folder initDestinationFolder(Session session, GedParameterDTO fileParameters) {

		String fullPath =
				"/Sites/acm/" + CommonConstantGED.DOCUMENT_LIBRARY + fileParameters.getPath();
		logger.info("GED destination Folder : full Path = {}", fullPath);
		// ACM/CLIENT_NAME
		Folder parentFolder = (Folder) session.getObjectByPath(fullPath);
		Folder destinationFolder = parentFolder;
		if (!ACMValidationUtils.isNullOrEmpty(fileParameters.getCustomerNumber())
				&& ACMValidationUtils.isNullOrEmpty(fileParameters.getLoanNumber())) {
			// Add file in : ACM/XXXX/acm_cutomer_xxxxxxx
			String folderName = CommonConstantGED.GED_PATH_PRODUCTION_ACM_CUSTOMER + "_"
					+ fileParameters.getCustomerNumber();
			destinationFolder =
					gedServiceFactory.createOrLoadFolder(session, parentFolder, folderName);
		}
		else if (!ACMValidationUtils.isNullOrEmpty(fileParameters.getCustomerNumber())
				&& !ACMValidationUtils.isNullOrEmpty(fileParameters.getLoanNumber())) {
			// Load Customer folder
			String customerFolderName = CommonConstantGED.GED_PATH_PRODUCTION_ACM_CUSTOMER + "_"
					+ fileParameters.getCustomerNumber();
			Folder customerDestinationFolder =
					gedServiceFactory.createOrLoadFolder(session, parentFolder, customerFolderName);

			// Add file in : ACM/XXXX/acm_cutomer_xxxxxxx/acm_loan_yyyyyy
			String loanFolderName = CommonConstantGED.GED_PATH_PRODUCTION_ACM_LOAN + "_"
					+ fileParameters.getLoanNumber();
			destinationFolder = gedServiceFactory.createOrLoadFolder(session,
					customerDestinationFolder, loanFolderName);
		}
		// return destinationFolder
		logger.info("Destination Folder= {}", destinationFolder);
		return destinationFolder;
	}

	/**
	 * Save files in ACM.
	 *
	 * @author HaythemBenizid
	 * @param fileToUpload the file to upload
	 * @param idDocument the id document
	 * @param loanId the loan id
	 * @param idCustomer the id customer
	 * @param acmDocumentsDTO the acm documents DTO
	 * @return the integer
	 */
	private Integer saveFilesInACM(File fileToUpload, Long idDocument, Long loanId, Long idCustomer,
			AcmDocumentsDTO acmDocumentsDTO) {

		// Check given params
		if (ACMValidationUtils.isNullOrEmpty(fileToUpload)
				&& ACMValidationUtils.isNullOrEmpty(idDocument)
				&& ACMValidationUtils.isNullOrEmpty(loanId)
				&& ACMValidationUtils.isNullOrEmpty(idCustomer)) {
			return 0;
		}
		// Check Documents is not already exist in Backup Table
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsGedService
				.find(new AcmDocumentsGedDTO(idDocument, loanId, idCustomer)))) {
			return 0;
		}
		try {
			// Creating bytearray of same length as file
			FileInputStream fileInputStream = null;
			byte[] bArray = new byte[(int) fileToUpload.length()];
			try {
				fileInputStream = new FileInputStream(fileToUpload);
				// Reading file content to byte array
				fileInputStream.read(bArray);
				fileInputStream.close();
			}
			catch (IOException ioExp) {
				ioExp.printStackTrace();
			}
			finally {
				if (fileInputStream != null) {
					try {
						fileInputStream.close();
					}
					catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
			// SAVE document in ACM DB
			AcmDocumentsGedDTO newAcmDocumentsGedDTO = acmDocumentsGedService
					.save(new AcmDocumentsGedDTO(idDocument, loanId, idCustomer, bArray));
			logger.warn("Document is inserted in ACM-DB as backup : {}", newAcmDocumentsGedDTO);

			return 1;
		}
		catch (Exception e) {
			logger.error("Exception has been occurred  while inserting file in DB : ", e);
			return 0;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#upload(List<MultipartFile>, List<String>, String)
	 */
	@Override
	public List<Document> upload(List<MultipartFile> multipartFiles, List<String> tags,
			String path) {

		List<Document> uploadedDocuments = new ArrayList<>();
		Session session = gedServiceFactory.createSession();
		// TODO in case we use more SITE
		// Folder site = gedServiceFactory.findSite(session)
		String fullPath = "/Sites/acm/" + CommonConstantGED.DOCUMENT_LIBRARY + path;
		try {
			Folder destinationFolder = (Folder) session.getObjectByPath(fullPath);
			List<File> files = new ArrayList<>();
			for (MultipartFile multipartFile : multipartFiles) {
				files.add(CommonFunctions.fileConverter(multipartFile,
						environment.getProperty("document.base.path")));
			}
			// upload document in GED
			for (File file : files) {
				uploadedDocuments.add(upload(file, destinationFolder, tags, session));
			}
		}
		catch (CmisObjectNotFoundException ex) {
			String message = "The path to save the artifact to ('{}') is not found.";
			logger.error(message, path, ex);
			ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
					CommonErrorCode.UPLOAD_FAILED, message,
					new TechnicalException(CommonExceptionsMessage.EXCEPTIONS_SITE_NOT_FOUND));
			logger.error("{}", exceptionResponseMessage);
			return new ArrayList<>();
		}
		catch (CmisRuntimeException ex) {
			String message = "Exception has been occurred  while uploading file.";
			logger.error(message, ex);
			ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
					CommonErrorCode.UPLOAD_FAILED, message,
					new TechnicalException(CommonExceptionsMessage.EXCEPTIONS_SITE_NOT_FOUND));
			logger.error("{}", exceptionResponseMessage);
			ex.printStackTrace();
			return new ArrayList<>();
		}
		logger.info("Returning list of uploaded Documents.");
		return uploadedDocuments;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#displayDocument(java.lang.String)
	 */
	@Override
	public byte[] displayDocument(String id) throws GEDException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		try {
			// Get Token from Ged To access
			String token = gedServiceFactory.findGedSiteToken();
			String uri = gedConfigProperties.getGedConfig(CommonConstantGED.URI_GED_NODES) + id
					+ "/content";
			// Set Headers
			HttpHeaders headers = new HttpHeaders();
			headers.set(CommonConstantGED.AUTHORIZATION, CommonConstantGED.BASIC + token);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_OCTET_STREAM));
			HttpEntity<String> entity = new HttpEntity<>(headers);
			RestTemplate restTemplate = new RestTemplate();
			restTemplate.getMessageConverters().add(new ByteArrayHttpMessageConverter());
			// Get File
			ResponseEntity<byte[]> response =
					restTemplate.exchange(uri, HttpMethod.GET, entity, byte[].class);
			logger.info("File Returned in byte[]");
			return response.getBody();
		}
		catch (Exception exp) {
			ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
					CommonErrorCode.DOCUMENT_ERROR,
					environment.getProperty("ged.document.found.error"), new TechnicalException(),
					Collections.singletonList(environment.getProperty("ged.document.error")));
			throw new GEDException(exceptionResponseMessage);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#downloadDocument(java.lang.String)
	 */
	@Override
	public Document downloadDocument(String id) throws GEDException {

		try {
			Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
			Session session = gedServiceFactory.createSession();
			return session.getLatestDocumentVersion(id);
		}
		catch (Exception exp) {
			ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
					CommonErrorCode.DOCUMENT_ERROR,
					environment.getProperty("ged.document.found.error"), new TechnicalException(),
					Collections.singletonList(environment.getProperty("ged.document.error")));
			throw new GEDException(exceptionResponseMessage);
		}
	}

	/**
	 * Find ged document by id.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the document
	 * @throws GEDException the GED exception
	 */
	public byte[] findGedDocumentById(String id) throws GEDException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		try {
			Session session = gedServiceFactory.createSession();
			CmisObject cmisObject = session.getObject(id);
			byte[] bytes = null;
			if (cmisObject instanceof Document) {
				Document document = (Document) cmisObject;
				logger.info("{}", document.getName());
				InputStream stream = document.getContentStream().getStream();

				try {
					bytes = IOUtils.toByteArray(stream);
				}
				finally {
					stream.close();
				}
			}
			else {
				logger.error("Object does not exist or is not a document");
				return bytes;
			}
			return bytes;
		}
		catch (Exception exp) {
			ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
					CommonErrorCode.DOCUMENT_ERROR,
					environment.getProperty("ged.document.found.error"), new TechnicalException(),
					Collections.singletonList(environment.getProperty("ged.document.error")));
			throw new GEDException(exceptionResponseMessage);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#findUserDoc()
	 */
	@SuppressWarnings({"unchecked", "rawtypes"})
	@Override
	public List<GedDocumentDTO> findByTag(String tag) throws GEDException {

		Preconditions.checkNotNull(tag, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		RestTemplate restTemplate = new RestTemplate();
		Map response = null;
		List<GedDocumentDTO> documentDTOs = new ArrayList<>();
		try {
			response = (Map) (restTemplate.exchange(
					gedConfigProperties.getGedConfig(CommonConstantGED.URI_GED_SEARCH_NODE)
							+ CommonConstantGED.TERM_EQUAL + tag + "&orderBy=createdAt desc",
					HttpMethod.GET, initHttpEntityConnection(), Map.class).getBody())
							.get(CommonConstantGED.LIST);
			List<LinkedHashMap<String, Map>> entries =
					(List) response.get(CommonConstantGED.ENTRIES);
			// parsing documents
			parsingDocuments(entries, documentDTOs);
		}
		catch (Exception exp) {
			ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
					CommonErrorCode.DOCUMENT_ERROR,
					environment.getProperty("ged.document.found.error"), new TechnicalException(),
					Collections.singletonList(environment.getProperty("ged.document.error")));
			throw new GEDException(exceptionResponseMessage);
		}
		return documentDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#find(java.lang.Boolean, java.util.List)
	 */
	@SuppressWarnings({"unchecked", "rawtypes"})
	@Override
	public List<GedDocumentDTO> find(Boolean all, List<String> tags) throws GEDException {

		Preconditions.checkNotNull(tags, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		List<GedDocumentDTO> documentDTOs = new ArrayList<>();
		try {
			Session session = gedServiceFactory.createSession();
			String token = gedServiceFactory.findGedSiteToken();
			RestTemplate restTemplate = new RestTemplate();

			HttpHeaders headers = new HttpHeaders();
			headers.set(CommonConstantGED.CONTENT_TYPE, CommonConstantGED.APPLICATION_JSON);
			headers.set(CommonConstantGED.AUTHORIZATION, CommonConstantGED.BASIC + token);

			// search query with given tags
			JSONObject jsonQuery = new JSONObject();
			JSONObject json = new JSONObject();

			// check list of tags
			if (ACMValidationUtils.isNullOrEmpty(tags)) {
				logger.warn("list of given tags is empty");
				return new ArrayList<>();
			}
			// building where clause using tags
			String searchQuery = "+TAG:" + tags.get(0);
			for (int i = 1; i < tags.size(); i++) {
				searchQuery = searchQuery.concat(" AND +TAG:" + tags.get(i));
			}
			jsonQuery.put("query", searchQuery);
			jsonQuery.put("language", "afts");
			json.put("query", jsonQuery);

			// query to order result documents by creation date desc
			JSONObject jsonSort = new JSONObject();
			jsonSort.put("type", "FIELD");
			jsonSort.put("field", "created");
			jsonSort.put("ascending", false);
			json.put("sort", jsonSort);

			// when all is false then we must restrict the number of reterned elements
			if (all != null && Boolean.FALSE.equals(all)) {
				// query to restrict the number of reterned elements
				JSONObject jsonPaging = new JSONObject();
				jsonPaging.put("maxItems", CommonConstantGED.MAX_ITEMS_VALUE);
				json.put("paging", jsonPaging);
			}
			Map response = null;
			HttpEntity<JSONObject> entity = new HttpEntity<>(json, headers);
			response = (Map) (restTemplate.postForEntity(
					gedConfigProperties.getGedConfig(CommonConstantGED.URI_GED_SEARCH_API), entity,
					Map.class)).getBody().get(CommonConstantGED.LIST);

			List<LinkedHashMap<String, Map>> entries =
					(List) response.get(CommonConstantGED.ENTRIES);
			entries.forEach(entry -> {
				Map documentObject = entry.get(CommonConstantGED.ENTRY);
				GedDocumentDTO documentDTO = new GedDocumentDTO(
						(documentObject.get(CommonConstantGED.CREATED_AT).toString()
								.split(CommonConstantGED.T))[0].replace(CommonConstantGED.TIR_6,
										CommonConstantGED.SLASH_PATH),
						documentObject.get(CommonConstantGED.IS_FOLDER).toString(),
						documentObject.get(CommonConstantGED.IS_FILE).toString(),
						(documentObject.get(CommonConstantGED.MODIFIED_AT).toString()
								.split(CommonConstantGED.T))[0].replace(CommonConstantGED.TIR_6,
										CommonConstantGED.SLASH_PATH),
						documentObject.get(CommonConstantGED.NAME).toString(),
						documentObject.get(CommonConstantGED.ID).toString(),
						((LinkedHashMap<String, String>) documentObject
								.get(CommonConstantGED.CONTENT)).get(CommonConstantGED.MIME_TYPE),
						documentObject.get(CommonConstantGED.PARENT_ID).toString());

				// setting document content in BYTE fromat
				try {
					documentDTO.setDocumentContentByte(displayDocument(documentDTO.getId()));
				}
				catch (GEDException e) {
					logger.error("{}", e.getMessage());
				}

				Document document = (Document) session.getObject(documentDTO.getId());
				String filePathComplet = document.getPaths().get(0);
				String[] filePath = filePathComplet.split(CommonConstantGED.DOCUMENT_LIBRARY);
				filePath = filePath[1].split(CommonConstantGED.SLASH_PATH + document.getName());
				filePath = filePath[0].split(CommonConstantGED.SLASH_PATH);
				documentDTO.setCategorie(filePath[0]);
				if (filePath.length == 2) {
					documentDTO.setParents(filePath[1]);
				}
				else if (filePath.length > 2) {
					documentDTO.setParents(filePath[1]);
					for (int i = 2; i < filePath.length; i++) {
						documentDTO.setParents(documentDTO.getParents() + " > " + filePath[i]);
					}
				}
				documentDTOs.add(documentDTO);
			});
		}
		catch (Exception exp) {
			ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
					CommonErrorCode.DOCUMENT_ERROR,
					environment.getProperty("ged.document.found.error"), new TechnicalException(),
					Collections.singletonList(environment.getProperty("ged.document.error")));
			throw new GEDException(exceptionResponseMessage);
		}
		return documentDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#find(java.lang.String)
	 */
	@SuppressWarnings({"unchecked", "rawtypes"})
	@Override
	public GedDocumentDTO find(String id) throws GEDException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Session session = null;
		try {
			session = gedServiceFactory.createSession();
		}
		catch (Exception e) {
			logger.error("CmisConnectionException : {}", e.getMessage());
			return new GedDocumentDTO();
		}
		RestTemplate restTemplate = new RestTemplate();
		Map response = null;
		try {
			response =
					(Map) (restTemplate
							.exchange(
									gedConfigProperties
											.getGedConfig(CommonConstantGED.URI_GED_NODES) + id,
									HttpMethod.GET, initHttpEntityConnection(), Map.class)
							.getBody()).get(CommonConstantGED.ENTRY);
		}
		catch (HttpClientErrorException exp) {
			if (exp.getRawStatusCode() == 404) {
				ExceptionResponseMessage exceptionResponseMessage =
						new ExceptionResponseMessage(CommonErrorCode.DOCUMENT_NOT_FOUND,
								environment.getProperty("ged.document.found.error"),
								new TechnicalException(), Collections.singletonList(
										environment.getProperty("ged.document.notFound")));
				throw new GEDException(exceptionResponseMessage);
			}
			else {
				ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
						CommonErrorCode.DOCUMENT_ERROR,
						environment.getProperty("ged.document.found.error"),
						new TechnicalException(),
						Collections.singletonList(environment.getProperty("ged.document.error")));
				throw new GEDException(exceptionResponseMessage);
			}
		}

		GedDocumentDTO documentDTO = new GedDocumentDTO(
				(response.get(CommonConstantGED.CREATED_AT).toString()
						.split(CommonConstantGED.T))[0].replace(CommonConstantGED.TIR_6,
								CommonConstantGED.SLASH_PATH),
				response.get(CommonConstantGED.IS_FOLDER).toString(),
				response.get(CommonConstantGED.IS_FILE).toString(),
				(response.get(CommonConstantGED.MODIFIED_AT).toString()
						.split(CommonConstantGED.T))[0].replace(CommonConstantGED.TIR_6,
								CommonConstantGED.SLASH_PATH),
				response.get(CommonConstantGED.NAME).toString(),
				response.get(CommonConstantGED.ID).toString(),
				((LinkedHashMap<String, String>) response.get(CommonConstantGED.CONTENT))
						.get(CommonConstantGED.MIME_TYPE),
				response.get(CommonConstantGED.PARENT_ID).toString());
		// setting document content in BYTE fromat
		documentDTO.setDocumentContentByte(displayDocument(id));

		Document document = (Document) session.getObject(documentDTO.getId());
		String filePathComplet = document.getPaths().get(0);
		String[] filePath = filePathComplet.split(CommonConstantGED.DOCUMENT_LIBRARY);
		filePath = filePath[1].split(CommonConstantGED.SLASH_PATH + document.getName());
		filePath = filePath[0].split(CommonConstantGED.SLASH_PATH);
		documentDTO.setCategorie(filePath[0]);
		return documentDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#delete(java.lang.String)
	 */
	@Override
	public Boolean delete(String idDocument) throws GEDException {

		Preconditions.checkNotNull(idDocument, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		try {
			if (Boolean.TRUE.equals(isExist(idDocument))) {
				Session session = gedServiceFactory.createSession();
				Document document = (Document) session.getObject(idDocument);
				String filePathComplet = document.getPaths().get(0);
				session.deleteByPath(filePathComplet, true);
				return true;
			}
			else {
				logger.error(CommonLoggerMessage.DOCUMENT_NOT_FOUND);
				String message = "Document not found";
				ExceptionResponseMessage exceptionResponseMessage =
						new ExceptionResponseMessage(CommonErrorCode.DELETE_FAILED, message,
								new TechnicalException(CommonExceptionsMessage.DOCUMENT_NOT_FOUND));
				throw new GEDException(exceptionResponseMessage);
			}
		}
		catch (Exception e) {
			String message = "Error while deleting document";
			ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
					CommonErrorCode.DELETE_FAILED, message,
					new TechnicalException(CommonExceptionsMessage.EXCEPTIONS_FAILED_DELETE));
			throw new GEDException(exceptionResponseMessage);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#isExist(String idDocument)
	 */
	@Override
	public Boolean isExist(String idDocumentGED) {

		// Check params
		if (ACMValidationUtils.isNullOrEmpty(idDocumentGED)) {
			return Boolean.FALSE;
		}
		try {
			Session session = gedServiceFactory.createSession();
			session.getObject(idDocumentGED);
			return Boolean.TRUE;
		}
		catch (CmisObjectNotFoundException e) {
			logger.error("Error while checking on document");
			return Boolean.FALSE;
		}
		catch (Exception e) {
			logger.error("CmisConnectionException : {}", e.getMessage());
			return Boolean.FALSE;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#isExist(com.acm.utils.dtos.AcmDocumentsDTO)
	 */
	@Override
	public Boolean isExist(AcmDocumentsDTO acmDocumentsDTO) {

		Preconditions.checkNotNull(acmDocumentsDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Boolean result = Boolean.FALSE;
		// Check params
		if (ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdDocumentGED())
				&& ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdDocument())) {
			return result;
		}

		// Check Document if exist in GED by given ID
		result = isExist(acmDocumentsDTO.getIdDocumentGED());

		// Check if document exist in Backup table
		if (Boolean.FALSE.equals(result)
				&& !ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdDocument())) {
			// Check if there is a document in AcmDocumentsGed Table in ACM-DB By given ID
			List<AcmDocumentsGedDTO> acmDocumentsGedDTOs = acmDocumentsGedService
					.find(new AcmDocumentsGedDTO(acmDocumentsDTO.getIdDocument()));
			if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsGedDTOs)) {
				// Documents exist in backUP
				result = Boolean.TRUE;
			}
		}
		logger.info("Returning Result : document exist : {} ", result);
		return result;
	}

	/**
	 * Upload file To Ged and add tags.
	 *
	 * @author HaythemBenizid
	 * @param file the file
	 * @param destinationFolder the destination folder
	 * @param tags the tags
	 * @param session the session
	 * @return the document
	 */
	private Document upload(File file, Folder destinationFolder, List<String> tags,
			Session session) {

		Preconditions.checkNotNull(file, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(destinationFolder,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		try {
			Map<String, Object> properties = new HashMap<>();
			properties.put(PropertyIds.NAME, file.getName());
			properties.put(PropertyIds.OBJECT_TYPE_ID, CommonConstantGED.CMIS_DOCUMENT);
			logger.info("File name : {} , path : {}", file.getName(), file.getPath());
			ContentStream contentStream =
					new ContentStreamImpl(file.getAbsolutePath(), BigInteger.valueOf(file.length()),
							CommonConstantGED.DEFAULT_MIMETYPE, new FileInputStream(file));

			// Send doc in GED
			Document document = destinationFolder.createDocument(properties, contentStream,
					VersioningState.MAJOR);
			logger.info("Doc id in GED : {}", document != null ? document.getId() : null);

			// save Tags in GED
			String token = gedServiceFactory.findGedSiteToken();
			for (String tag : tags) {
				gedServiceFactory.addTag(document, tag, token);
			}

			logger.info("uplaod Doc in GED :: DONE");
			session.clear();
			Path pathFile = Paths.get(file.getAbsolutePath());
			cleanUp(pathFile);
			return document;
		}
		catch (CmisContentAlreadyExistsException | FileNotFoundException e) {
			String message =
					"Exception has been occurred  while uploading file : FILE EXIST ALREADY Or NOT FOUND";
			ExceptionResponseMessage exceptionResponseMessage =
					new ExceptionResponseMessage(CommonErrorCode.SAVE_FILE_ALREADY_EXIST, message,
							new TechnicalException(CommonErrorCode.SAVE_FILE_ALREADY_EXIST,
									CommonExceptionsMessage.FILE_EXIST_ALREADY,
									this.getClass().getName()));
			logger.error("{}", exceptionResponseMessage);
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * delete file from temp with given path.
	 * 
	 * @author AbdelkarimTurki
	 * @param path the path
	 */
	public void cleanUp(Path path) {

		try {
			Files.delete(path);
		}
		catch (IOException e) {
			logger.error("Error : Failed to delete file {} from temp", path);
		}
	}

	/**
	 * set properties for the list of uploaded documents.
	 * 
	 * @author HaythemBenizid
	 * @param entries {@link List} of entries to set for the documents
	 * @param documentDTOs {@link GedDocumentDTO} the list of documentDTO
	 */
	@SuppressWarnings({"unchecked", "rawtypes"})
	private void parsingDocuments(List<LinkedHashMap<String, Map>> entries,
			List<GedDocumentDTO> documentDTOs) {

		try {
			Session session = gedServiceFactory.createSession();
			entries.forEach(entry -> {
				Map<?, ?> documentObject = entry.get(CommonConstantGED.ENTRY);
				GedDocumentDTO documentDTO = new GedDocumentDTO(
						(documentObject.get(CommonConstantGED.CREATED_AT).toString()
								.split(CommonConstantGED.T))[0].replace(CommonConstantGED.TIR_6,
										CommonConstantGED.SLASH_PATH),
						documentObject.get(CommonConstantGED.IS_FOLDER).toString(),
						documentObject.get(CommonConstantGED.IS_FILE).toString(),
						(documentObject.get(CommonConstantGED.MODIFIED_AT).toString()
								.split(CommonConstantGED.T))[0].replace(CommonConstantGED.TIR_6,
										CommonConstantGED.SLASH_PATH),
						documentObject.get(CommonConstantGED.NAME).toString(),
						documentObject.get(CommonConstantGED.ID).toString(),
						((LinkedHashMap<String, String>) documentObject
								.get(CommonConstantGED.CONTENT)).get(CommonConstantGED.MIME_TYPE),
						documentObject.get(CommonConstantGED.PARENT_ID).toString());
				/*
				 * setting document content in BYTE fromat
				 */
				try {
					documentDTO.setDocumentContentByte(displayDocument(documentDTO.getId()));
				}
				catch (GEDException e) {
					logger.error("GEDException : {}", e.getMessage());
				}

				Document document = (Document) session.getObject(documentDTO.getId());
				String filePathComplet = document.getPaths().get(0);
				String[] filePath = filePathComplet.split(CommonConstantGED.DOCUMENT_LIBRARY);
				filePath = filePath[1].split(CommonConstantGED.SLASH_PATH + document.getName());
				filePath = filePath[0].split(CommonConstantGED.SLASH_PATH);
				documentDTO.setCategorie(filePath[0]);
				if (filePath.length == 2) {
					documentDTO.setParents(filePath[1]);
				}
				else if (filePath.length > 2) {
					documentDTO.setParents(filePath[1]);
					for (int i = 2; i < filePath.length; i++) {
						documentDTO.setParents(documentDTO.getParents() + " > " + filePath[i]);
					}
				}
				documentDTOs.add(documentDTO);
			});
		}
		catch (Exception exp) {
			logger.error("Failed to parsingDocuments : {}", exp.getMessage());
		}
	}

	/**
	 * Inits the http entity connection.
	 * 
	 * @author HaythemBenizid
	 * @return the http entity
	 */
	@SuppressWarnings({"unchecked", "rawtypes"})
	private HttpEntity initHttpEntityConnection() {

		String token = gedServiceFactory.findGedSiteToken();
		HttpHeaders headers = new HttpHeaders();
		headers.set(CommonConstantGED.AUTHORIZATION, CommonConstantGED.BASIC + token);

		return new HttpEntity(headers);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.DocumentService#findImageForCustomer(java.lang.String)
	 */
	@Override
	public GedDocumentDTO findImageForCustomer(String tag) throws GEDException {

		// initialize the gedDocumentDTO that will be returned by this function
		GedDocumentDTO gedDocumentDTO = new GedDocumentDTO();
		// find all documents with the tag in parameter
		List<GedDocumentDTO> listGedDocumentsDTO = findByTag(tag);

		if (!ACMValidationUtils.isNullOrEmpty(listGedDocumentsDTO)
				&& !ACMValidationUtils.isNullOrEmpty(listGedDocumentsDTO.get(0).getId())
				&& !ACMValidationUtils.isNullOrEmpty(listGedDocumentsDTO.get(0).getMimeType())) {
			// set the byte[] customerImage with the document found in GED
			gedDocumentDTO
					.setDocumentContentByte(displayDocument(listGedDocumentsDTO.get(0).getId()));
			// set the type of customerImage with the type of document found in GED
			gedDocumentDTO.setMimeType(listGedDocumentsDTO.get(0).getMimeType());
		}
		return gedDocumentDTO;
	}
}
