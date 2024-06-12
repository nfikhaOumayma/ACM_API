/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistorySetting;
import com.acm.client.CreditClient;
import com.acm.client.ExpensesClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ItemRepository;
import com.acm.repository.SettingDocumentTypeRepository;
import com.acm.repository.SettingWorkFlowRepository;
import com.acm.service.ProductService;
import com.acm.service.SettingDocumentProductService;
import com.acm.service.SettingDocumentTypeService;
import com.acm.service.SettingHistoriqueService;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.enums.DocumentTypeCatgory;
import com.acm.utils.models.QSettingDocumentType;
import com.acm.utils.models.SettingDocumentType;
import com.acm.utils.models.WorkFlowStep;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingDocumentTypeServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Service
public class SettingDocumentTypeServiceImpl implements SettingDocumentTypeService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingDocumentTypeServiceImpl.class);

	/** The settingDocumentType repository. */
	@Autowired
	private SettingDocumentTypeRepository settingDocumentTypeRepository;

	/** The product repository. */
	@Autowired
	private ProductService productService;

	/** The settingDocumentProduct repository. */
	@Autowired
	private SettingDocumentProductService settingDocumentProductService;

	/** The setting historique service. */
	@Autowired
	private SettingHistoriqueService settingHistoriqueService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The expenses client. */
	@Autowired
	private ExpensesClient expensesClient;

	/** The setting work flow repository. */
	@Autowired
	private SettingWorkFlowRepository settingWorkFlowRepository;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The item repository. */
	@Autowired
	private ItemRepository itemRepository;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentTypeService#find(com.acm.utils.dtos.
	 * SettingDocumentTypeDTO)
	 */
	@Override
	public List<SettingDocumentTypeDTO> find(SettingDocumentTypeDTO settingDocumentTypeDTO) {

		// init QSettingDocumentType
		QSettingDocumentType qSettingDocumentType = QSettingDocumentType.settingDocumentType;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qSettingDocumentType.enabled.eq(Boolean.TRUE));

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(settingDocumentTypeDTO.getId())) {
			predicate.and(qSettingDocumentType.id.eq(settingDocumentTypeDTO.getId()));
		}

		// find by code
		if (!ACMValidationUtils.isNullOrEmpty(settingDocumentTypeDTO.getCode())) {
			predicate.and(qSettingDocumentType.code.eq(settingDocumentTypeDTO.getCode()));
		}

		// find by categorie LOAN = 0 / CLIENT = 1 / ASSIGN_DOCUMENT = 2
		if (!ACMValidationUtils.isNullOrEmpty(settingDocumentTypeDTO.getCategorie())) {
			predicate.and(qSettingDocumentType.categorie.eq(settingDocumentTypeDTO.getCategorie()));
		}

		// QueryDSL using springDATA
		Iterable<SettingDocumentType> iterable = settingDocumentTypeRepository.findAll(predicate);
		List<SettingDocumentType> settingDocumentTypes = new ArrayList<>();
		iterable.forEach(settingDocumentTypes::add);
		logger.info("{} : setting Documents type was founded", settingDocumentTypes.size());

		// mapping returned list
		List<SettingDocumentTypeDTO> settingDocumentTypeDTOs = new ArrayList<>();
		settingDocumentTypes.forEach(settingDocumentType -> settingDocumentTypeDTOs
				.add(mapper.map(settingDocumentType, SettingDocumentTypeDTO.class)));

		logger.info("Returning founded data ...");
		return settingDocumentTypeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentTypeService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingDocumentTypeDTO)
	 */
	@Override
	public SettingDocumentTypeDTO save(Long id, SettingDocumentTypeDTO settingDocumentTypeDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingDocumentTypeDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update settingDocumentType  with ID = {}", id);
		SettingDocumentType oldSettingDocumentType =
				settingDocumentTypeRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(oldSettingDocumentType)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingDocumentType.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingDocumentType.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(SettingDocumentType.class),
				CommonAOPConstants.UPDATE, id, CommonFunctions.convertObjectToJSONString(
						mapper.map(oldSettingDocumentType, SettingDocumentTypeDTO.class)));

		// mapping new data with existing data (oldSettingDocumentType)
		mapper.map(settingDocumentTypeDTO, oldSettingDocumentType);
		CommonFunctions.mapperToUpdate(oldSettingDocumentType, userClient, logger);
		SettingDocumentType newSettingDocumentType =
				settingDocumentTypeRepository.save(oldSettingDocumentType);
		SettingDocumentTypeDTO newSettingDocumentTypeDTO =
				mapper.map(newSettingDocumentType, SettingDocumentTypeDTO.class);

		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newSettingDocumentType.getUpdatedBy());
		settingHistoriqueDTO
				.setNewData(CommonFunctions.convertObjectToJSONString(newSettingDocumentTypeDTO));
		settingHistoriqueService.save(settingHistoriqueDTO);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				SettingDocumentType.class.getSimpleName());
		// update document name if exist in expenses type
		expensesClient.updateDocumentName(newSettingDocumentType.getLibelle(),
				newSettingDocumentType.getId());
		return mapper.map(newSettingDocumentType, SettingDocumentTypeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.SettingDocumentTypeService#save(com.acm.utils.dtos.SettingDocumentTypeDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public SettingDocumentTypeDTO save(SettingDocumentTypeDTO settingDocumentTypeDTO) {

		Preconditions.checkNotNull(settingDocumentTypeDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingDocumentType settingDocumentType =
				mapper.map(settingDocumentTypeDTO, SettingDocumentType.class);
		CommonFunctions.mapperToSave(settingDocumentType, userClient, logger);
		SettingDocumentType newSettingDocumentType =
				settingDocumentTypeRepository.save(settingDocumentType);
		SettingDocumentTypeDTO newsettingDocumentTypeDTO =
				mapper.map(newSettingDocumentType, SettingDocumentTypeDTO.class);
		try {
			// create Setting Document Product
			createSettingDocumentProduct(newsettingDocumentTypeDTO);
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingDocumentType.class.getSimpleName());
		}
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				SettingDocumentType.class.getSimpleName());
		return mapper.map(newSettingDocumentType, SettingDocumentTypeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentTypeService#find(java.lang.Long)
	 */
	@Override
	public SettingDocumentTypeDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find settingDocumentType by ID : {}", id);
		SettingDocumentType settingDocumentType =
				settingDocumentTypeRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(settingDocumentType)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingDocumentType.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ SettingDocumentType.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(settingDocumentType, SettingDocumentTypeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentTypeService#find()
	 */
	@Override
	public List<SettingDocumentTypeDTO> find() {

		List<SettingDocumentType> settingDocumentTypes = settingDocumentTypeRepository.findAll();
		List<SettingDocumentTypeDTO> settingDocumentTypesDTOs = new ArrayList<>();
		settingDocumentTypes.forEach(settingDocumentType -> settingDocumentTypesDTOs
				.add(mapper.map(settingDocumentType, SettingDocumentTypeDTO.class)));
		return settingDocumentTypesDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentTypeService#updateStatusDocumentType(com.acm.utils.dtos.
	 * SettingDocumentTypeDTO)
	 */
	@Override
	public void updateStatus(SettingDocumentTypeDTO settingDocumentTypeDTO, Boolean status)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(settingDocumentTypeDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(settingDocumentTypeDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(status, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.warn("disable settingDocumentTypeDTO  with ID = {}", settingDocumentTypeDTO.getId());
		// update status object by id
		SettingDocumentTypeDTO newSettingDocumentTypeDTO = find(settingDocumentTypeDTO.getId());
		if (Boolean.TRUE.equals(status)) {
			newSettingDocumentTypeDTO.setEnabled(Boolean.TRUE);
		}
		else {
			newSettingDocumentTypeDTO.setEnabled(Boolean.FALSE);
			disableSettingDocumentProduct(settingDocumentTypeDTO);
		}
		save(newSettingDocumentTypeDTO.getId(), newSettingDocumentTypeDTO);
		logger.info("update status Document Type with ID = {} :: DONE",
				settingDocumentTypeDTO.getId());

	}

	/**
	 * Disable setting document product.
	 *
	 * @author AbdelkarimTurki
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void disableSettingDocumentProduct(SettingDocumentTypeDTO settingDocumentTypeDTO)
			throws ResourcesNotFoundException {

		SettingDocumentProductDTO settingDocumentProductDTO = new SettingDocumentProductDTO();
		settingDocumentProductDTO.setSettingDocumentTypeDTO(settingDocumentTypeDTO);
		List<SettingDocumentProductDTO> settingDocumentProductDTOs =
				settingDocumentProductService.find(settingDocumentProductDTO);
		if (!ACMValidationUtils.isNullOrEmpty(settingDocumentProductDTOs)) {
			for (SettingDocumentProductDTO settingDocumentProduct : settingDocumentProductDTOs) {
				settingDocumentProductService.updateStatus(settingDocumentProduct, Boolean.FALSE);
			}
		}
	}

	/**
	 * Creates the setting document product.
	 *
	 * @author AbdelkarimTurki
	 * @param settingDocumentTypeDTO the setting document type DTO
	 */
	private void createSettingDocumentProduct(SettingDocumentTypeDTO settingDocumentTypeDTO) {

		ProductDTO productDTO = new ProductDTO();
		List<ProductDTO> productDTOs = productService.findAll(productDTO);

		for (ProductDTO product : productDTOs) {
			settingDocumentProductService.save(new SettingDocumentProductDTO(settingDocumentTypeDTO,
					product.getId().intValue(), Boolean.FALSE));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentTypeService#findDocTypeByStep(java.lang.Long,
	 * java.lang.Long)
	 */
	@Override
	public List<SettingDocumentTypeDTO> findDocTypeByStep(Long idStep, Long idInstance) {

		WorkFlowStep workFlowStep = settingWorkFlowRepository.findById(idStep).get();
		List<SettingDocumentTypeDTO> settingDocumentTypeDTOs = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(workFlowStep)) {
			workFlowStep.getDocumentTypes().forEach(element -> {
				settingDocumentTypeDTOs.add(mapper.map(element, SettingDocumentTypeDTO.class));
			});
		}

		AcmDocumentsDTO acmDocumentsDTO = new AcmDocumentsDTO();
		acmDocumentsDTO.setItemInstanceId(idInstance);
		List<AcmDocumentsDTO> acmDocumentsDTOs = creditClient.find(acmDocumentsDTO);
		settingDocumentTypeDTOs.forEach(element -> {
			acmDocumentsDTOs.forEach(acmDoc -> {
				if (element.getId().equals(acmDoc.getSettingDocumentTypeDTO().getId())) {
					element.setIdDocumentGED(acmDoc.getIdDocumentGED());
					element.setName(acmDoc.getName());
				}
			});

		});

		return settingDocumentTypeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentTypeService#findDocByCategory(java.lang.String)
	 */
	@Override
	public List<SettingDocumentTypeDTO> findDocByCategory(String category, Long elementId) {

		List<List<SettingDocumentType>> settingDocumentTypes =
				Arrays.stream(DocumentTypeCatgory.values())
						.filter(documentType -> documentType.name().equalsIgnoreCase(category))
						.map(documentType -> settingDocumentTypeRepository
								.findByCategorie(documentType.categoryId()))
						.filter(Objects::nonNull).collect(Collectors.toList());
		List<SettingDocumentTypeDTO> settingDocumentTypeDTOs = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(settingDocumentTypes)) {
			settingDocumentTypes.get(0).forEach(element -> {
				settingDocumentTypeDTOs.add(mapper.map(element, SettingDocumentTypeDTO.class));
			});
		}

		AcmDocumentsDTO acmDocumentsDTO = new AcmDocumentsDTO();
		acmDocumentsDTO.setCategory(category);
		acmDocumentsDTO.setElementId(elementId);
		List<AcmDocumentsDTO> acmDocumentsDTOs = creditClient.find(acmDocumentsDTO);
		settingDocumentTypeDTOs.forEach(element -> {
			acmDocumentsDTOs.forEach(acmDoc -> {
				if (element.getId().equals(acmDoc.getSettingDocumentTypeDTO().getId())) {
					element.setIdDocumentGED(acmDoc.getIdDocumentGED());
					element.setName(acmDoc.getName());
					element.setDateDebut(acmDoc.getDateCreation());
				}
			});

		});

		return settingDocumentTypeDTOs;

	}

}
