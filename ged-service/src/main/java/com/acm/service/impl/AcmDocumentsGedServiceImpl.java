/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.acm.client.CreditClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstantGED;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmDocumentsGedRepository;
import com.acm.service.AcmDocumentsGedService;
import com.acm.service.DocumentService;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.AcmDocumentsGedDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.models.AcmDocumentsGed;
import com.acm.utils.models.QAcmDocumentsGed;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

import feign.FeignException;

/**
 * {@link AcmDocumentsGedServiceImpls} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Service
public class AcmDocumentsGedServiceImpl implements AcmDocumentsGedService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmDocumentsGedServiceImpl.class);

	/** The acmDocumentsGed repository. */
	@Autowired
	private AcmDocumentsGedRepository acmDocumentsGedRepository;

	/** The document service. */
	@Autowired
	private DocumentService documentService;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsGedService#find(java.lang.Integer)
	 */
	@Override
	public AcmDocumentsGedDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find AcmDocumentsGED by ID : {}", id);
		AcmDocumentsGed acmDocumentsGed = acmDocumentsGedRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(acmDocumentsGed)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmDocumentsGed.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ AcmDocumentsGed.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(acmDocumentsGed, AcmDocumentsGedDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsGedService#countAll()
	 */
	@Override
	public Long countAll() {

		return acmDocumentsGedRepository.count();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsGedService#find(com.acm.utils.dtos.AcmDocumentsGedDTO)
	 */
	@Override
	public List<AcmDocumentsGedDTO> find(AcmDocumentsGedDTO acmDocumentsGedDTO) {

		if (ACMValidationUtils.isNullOrEmpty(acmDocumentsGedDTO)) {
			return new ArrayList<>();
		}
		// init Predicate
		BooleanBuilder predicate = buildQuery(acmDocumentsGedDTO);

		// QueryDSL using springDATA
		Iterable<AcmDocumentsGed> iterable = acmDocumentsGedRepository.findAll(predicate);
		List<AcmDocumentsGed> acmDocumentsGeds = new ArrayList<>();
		iterable.forEach(acmDocumentsGeds::add);
		logger.info("{} : Documents GED was founded", acmDocumentsGeds.size());

		// mapping returned list
		List<AcmDocumentsGedDTO> acmDocumentsGedDTOs = new ArrayList<>();
		acmDocumentsGeds.forEach(acmDocumentsGed -> acmDocumentsGedDTOs
				.add(mapper.map(acmDocumentsGed, AcmDocumentsGedDTO.class)));

		logger.info("Returning founded data ...");
		return acmDocumentsGedDTOs;
	}

	/**
	 * Builds the query.
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsGedDTO the acm documents DTO
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(AcmDocumentsGedDTO acmDocumentsGedDTO) {

		// init QAcmDocuments
		QAcmDocumentsGed qAcmDocumentsGed = QAcmDocumentsGed.acmDocumentsGed;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by IdCustomer
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsGedDTO.getIdCustomer())) {
			predicate.and(qAcmDocumentsGed.idCustomer.eq(acmDocumentsGedDTO.getIdCustomer()));
		}

		// find by Loan ID
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsGedDTO.getLoanId())) {
			predicate.and(qAcmDocumentsGed.loanId.eq(acmDocumentsGedDTO.getLoanId()));
		}

		// find by id document ACM
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsGedDTO.getIdDocument())) {
			predicate.and(qAcmDocumentsGed.idDocument.eq(acmDocumentsGedDTO.getIdDocument()));
		}

		// find only enabled data
		predicate.and(qAcmDocumentsGed.enabled.eq(Boolean.TRUE));

		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsGedService#save(com.acm.utils.dtos.AcmDocumentsGedDTO)
	 */
	@Override
	public AcmDocumentsGedDTO save(AcmDocumentsGedDTO acmDocumentsGedDTO) {

		Preconditions.checkNotNull(acmDocumentsGedDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmDocumentsGed acmDocumentsGed = mapper.map(acmDocumentsGedDTO, AcmDocumentsGed.class);
		CommonFunctions.mapperToSave(acmDocumentsGed, userClient, logger);
		AcmDocumentsGed newDocumentsLoan = acmDocumentsGedRepository.save(acmDocumentsGed);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmDocumentsGed.class.getSimpleName());
		return mapper.map(newDocumentsLoan, AcmDocumentsGedDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsGedService#save(java.lang.Long,
	 * com.acm.utils.dtos.AcmDocumentsGedDTO)
	 */
	@Override
	public AcmDocumentsGedDTO save(Long id, AcmDocumentsGedDTO acmDocumentsGedDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmDocumentsGedDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Update AcmDocumentGed with ID = {}", id);
		AcmDocumentsGed oldAcmDocumentGed = acmDocumentsGedRepository.findById(id).orElse(null);

		// check if object is null
		if (oldAcmDocumentGed == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmDocumentsGed.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + AcmDocumentsGed.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldAcmDocumentGed)
		mapper.map(acmDocumentsGedDTO, oldAcmDocumentGed);
		CommonFunctions.mapperToUpdate(oldAcmDocumentGed, userClient, logger);

		// update & persist data in DB
		AcmDocumentsGed newAcmDocumentGed = acmDocumentsGedRepository.save(oldAcmDocumentGed);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmDocumentsGed.class.getSimpleName());
		return mapper.map(newAcmDocumentGed, AcmDocumentsGedDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsGedService#delete(com.acm.utils.dtos.AcmDocumentsGedDTO)
	 */
	@Override
	public void delete(AcmDocumentsGedDTO acmDocumentsGedDTO) {

		Preconditions.checkNotNull(acmDocumentsGedDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(acmDocumentsGedDTO.getIdDocument(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("delete acmDocumentsGED  with ID = {}", acmDocumentsGedDTO.getIdDocument());
		// delete object by id
		AcmDocumentsGed acmDocumentsGed = new AcmDocumentsGed();
		acmDocumentsGed.setId(acmDocumentsGedDTO.getId());
		acmDocumentsGedRepository.delete(acmDocumentsGed);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, AcmDocumentsGed.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsGedService#disableAll()
	 */
	@Override
	public Integer disableAll() {

		Integer count = 0;
		// find all acmDocumentsGed where idDocuemt not equal to 0L (the id of document photo
		// client)
		QAcmDocumentsGed qAcmDocumentsGed = QAcmDocumentsGed.acmDocumentsGed;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find list of acmDocumentsGed where idDocument not equal 0L
		predicate.and(qAcmDocumentsGed.idDocument.ne(0L));

		// QueryDSL using springDATA
		Iterable<AcmDocumentsGed> iterable = acmDocumentsGedRepository.findAll(predicate);
		List<AcmDocumentsGed> acmDocumentsGeds = new ArrayList<>();
		iterable.forEach(acmDocumentsGeds::add);
		logger.info("{} : Documents GED was founded", acmDocumentsGeds.size());
		List<AcmDocumentsGedDTO> acmDocumentsGedDTOs = new ArrayList<>();
		acmDocumentsGeds.forEach(acmDocumentsGed -> acmDocumentsGedDTOs
				.add(mapper.map(acmDocumentsGed, AcmDocumentsGedDTO.class)));
		// processing data
		if (!acmDocumentsGedDTOs.isEmpty()) {
			for (AcmDocumentsGedDTO acmDocumentsGedDTO : acmDocumentsGedDTOs) {

				// Send document to GED
				String idGed = sendToGed(acmDocumentsGedDTO);
				// disable given document
				if (!ACMValidationUtils.isNullOrEmpty(idGed)
						|| acmDocumentsGedDTO.getDocumentGed().length == 0) {
					try {
						delete(acmDocumentsGedDTO);
						count++;
					}
					catch (Exception e) {
						logger.error("Failed to delete Data");
						e.printStackTrace();
					}
				}
			}
		}
		logger.info(" {} AcmDocumentsGed was DELETED.", count);
		return count;
	}

	/**
	 * Send backup document to GED.
	 *
	 * @author HaythemBenizid
	 * @param acmDocumentsGedDTO the acm documents ged DTO
	 * @return the string
	 */
	private String sendToGed(AcmDocumentsGedDTO acmDocumentsGedDTO) {

		if (acmDocumentsGedDTO != null && acmDocumentsGedDTO.getIdDocument() != null
				&& acmDocumentsGedDTO.getDocumentGed().length > 0) {
			// init data to be send to GED
			GedParameterDTO gedParameterDTO = new GedParameterDTO();
			gedParameterDTO.setPath(CommonConstants.APP_CLIENT);
			gedParameterDTO.setSite(CommonConstantGED.ACM_SITE);
			try {
				// find acmDocumentDTO by ID
				AcmDocumentsDTO acmDocumentsDTO =
						creditClient.findAcmDocumentsById(acmDocumentsGedDTO.getIdDocument());
				// setting tags
				List<String> tags = new ArrayList<>();
				tags.add(acmDocumentsDTO.getTitre());
				if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getLoanId())) {
					// find data of loan
					LoanDTO loanDTO = creditClient.findLoanById(acmDocumentsDTO.getLoanId());
					tags.add(loanDTO.getAccountNumber());
					tags.add(loanDTO.getProductCode());
					// set loan number
					gedParameterDTO.setLoanNumber(loanDTO.getAccountNumber());
					// set customer number
					gedParameterDTO.setCustomerNumber(loanDTO.getCustomerDTO().getCustomerNumber());
					// setting data for backup if there is an exception
					gedParameterDTO.setIdCustomer(loanDTO.getCustomerDTO().getId());
					gedParameterDTO.setIdDocument(acmDocumentsDTO.getIdDocument());
					gedParameterDTO.setLoanId(loanDTO.getLoanId());
				}
				else if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdCustomer())) {
					// find customer data
					CustomerDTO customerDTO =
							creditClient.findCustomerById(acmDocumentsDTO.getIdCustomer());
					tags.add(customerDTO.getCustomerNumber());
					// set customer number
					gedParameterDTO.setCustomerNumber(customerDTO.getCustomerNumber());
					// setting data for backup if there is an exception
					gedParameterDTO.setIdCustomer(acmDocumentsDTO.getIdCustomer());
					gedParameterDTO.setIdDocument(acmDocumentsDTO.getIdDocument());
				}
				gedParameterDTO.setTags(tags);
				// convert MultipartFile to File
				List<File> filesToSend = new ArrayList<>();
				logger.debug("Begin convert Byte[] (Array) to File");
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(
						"yyyy-MM-dd_HH-mm-ss_SSS" + (new Random().nextInt(9999 - 999) + 999));
				File convertedFile =
						new File(environment.getProperty("spring.servlet.multipart.location") + "/"
								+ LocalDateTime.now().format(formatter));
				FileOutputStream fileOutputStream = null;
				try {
					boolean createdFile = convertedFile.createNewFile();
					if (createdFile) {
						fileOutputStream = new FileOutputStream(convertedFile);
						fileOutputStream.write(acmDocumentsGedDTO.getDocumentGed());
						fileOutputStream.close();
					}
				}
				catch (IOException e) {
					logger.error("Exception has been occurred  while zipping file : {}",
							e.getMessage());
				}
				finally {
					try {
						if (fileOutputStream != null) {
							fileOutputStream.close();
						}
					}
					catch (IOException e) {
						logger.error("Exception has been occurred  while closing stream file : {}",
								e.getMessage());
					}
				}
				logger.debug("Finish convert");
				filesToSend.add(convertedFile);

				gedParameterDTO.setFilesToUpload(filesToSend);
				try {
					// send to GED
					List<String> ids = documentService.uploadToGed(gedParameterDTO);
					String idDocumentGED =
							ACMValidationUtils.isNullOrEmpty(ids) ? null : ids.get(0).split(";")[0];
					logger.info("idDocumentGed inserted : {} ", idDocumentGED);
					// update data : id DocumentGED if is not NULL
					if (!ACMValidationUtils.isNullOrEmpty(idDocumentGED)) {
						acmDocumentsDTO.setIdDocumentGED(idDocumentGED);
						// update ID GED in ACM-DB
						creditClient.updateAcmDocuments(acmDocumentsDTO);
						// save data document customer in DB
						if (acmDocumentsDTO.getSettingDocumentTypeDTO()
								.getCategorie() == CommonFunctions
										.mappingStatus(CommonConstants.ACM_SETTING_DOCS_TYPE_CLIENT)
										.getKey()) {
							// find by id customer and Id GED is null
							AcmDocumentsDTO params = new AcmDocumentsDTO();
							params.setProcessLoanDocuments(Boolean.TRUE);
							params.setIdCustomer(acmDocumentsDTO.getIdCustomer());
							params.setSettingDocumentTypeDTO(
									acmDocumentsDTO.getSettingDocumentTypeDTO());
							List<AcmDocumentsDTO> acmDocumentsDTOs = creditClient.find(params);
							if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTOs)) {
								acmDocumentsDTOs.get(0).setIdDocumentGED(idDocumentGED);
								// update ID GED for customer document
								creditClient.updateAcmDocuments(acmDocumentsDTOs.get(0));
							}
						}
					}
					return idDocumentGED;
				}
				catch (Exception e) {
					logger.error("failed to save in GED");
					logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
							e.getMessage());
					return null;
				}
			}
			catch (FeignException e) {
				logger.error("Failed to get Document DATA from ACM DB");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AcmDocumentsGedService#uploadClientPhoto(org.springframework.web.multipart.
	 * MultipartFile, com.acm.utils.dtos.AcmDocumentsGedDTO)
	 */
	@Override
	public byte[] uploadClientPhoto(MultipartFile photo, Long idDocument)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(idDocument, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Document by ID : {}", idDocument);
		// find acmDocumentGed by documentId
		AcmDocumentsGedDTO documentClient = new AcmDocumentsGedDTO();
		// set documentId
		documentClient.setIdDocument(idDocument);
		// find client photo
		List<AcmDocumentsGedDTO> documentsGed = find(documentClient);

		if (ACMValidationUtils.isNullOrEmpty(documentsGed)) {
			try {
				// set the new photo in documentGed
				documentClient.setDocumentGed(photo.getBytes());
				documentClient.setIdCustomer(0L);
				documentClient.setLoanId(0L);
				return save(documentClient).getDocumentGed();
			}
			catch (IOException e) {
				logger.error("Error Saving PHOTO to client : {}", idDocument);
			}
		}
		else {
			// update
			AcmDocumentsGedDTO oldDocumentClient = documentsGed.get(0);
			try {
				oldDocumentClient.setDocumentGed(photo.getBytes());
				return save(oldDocumentClient.getId(), oldDocumentClient).getDocumentGed();
			}
			catch (IOException e) {
				logger.error("Error Saving PHOTO to client : {}", idDocument);
			}
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsGedService#findPhotoClient(java.lang.Long)
	 */
	@Override
	public byte[] findPhotoClient(Long idDocument) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(idDocument, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Document by ID : {}", idDocument);
		// find the photo client by idDocument (idDocument : 0L)
		AcmDocumentsGedDTO documentClient = new AcmDocumentsGedDTO();
		documentClient.setIdDocument(idDocument);
		// find client photo
		List<AcmDocumentsGedDTO> documentsGed = find(documentClient);
		if (!ACMValidationUtils.isNullOrEmpty(documentsGed)) {
			// get the first element
			AcmDocumentsGedDTO clientDocument = documentsGed.get(0);
			return clientDocument.getDocumentGed();
		}
		return new byte[0];
	}
}
