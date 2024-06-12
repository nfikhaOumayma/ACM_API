/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.type.ExpensesTypeUnicityCodeException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ExpensesTypeRepository;
import com.acm.service.ExpensesTypeService;
import com.acm.utils.dtos.ExpensesTypeDTO;
import com.acm.utils.models.ExpensesType;
import com.acm.utils.models.QExpensesType;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link ExpensesTypeServiceImpl} class.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
@Service
public class ExpensesTypeServiceImpl implements ExpensesTypeService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(ExpensesTypeServiceImpl.class);

	/** The expenses type repository. */
	@Autowired
	private ExpensesTypeRepository expensesTypeRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesTypeService#findAll()
	 */
	@Override
	public List<ExpensesTypeDTO> findAll() {

		logger.info("start find all expenses Types ...");
		// find all expenses Types
		List<ExpensesType> expensesTypes = expensesTypeRepository.findAll();
		// mapping returned list
		List<ExpensesTypeDTO> expensesTypeDTOs = new ArrayList<>();
		expensesTypes.forEach(expensesType -> expensesTypeDTOs
				.add(mapper.map(expensesType, ExpensesTypeDTO.class)));

		logger.info("Returning founded expenses Types with size = {}", expensesTypeDTOs.size());
		return expensesTypeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesTypeService#save(com.acm.utils.dtos.ExpensesTypeDTO)
	 */
	@Override
	public ExpensesTypeDTO save(ExpensesTypeDTO expensesTypeDTO)
			throws ExpensesTypeUnicityCodeException {

		Preconditions.checkNotNull(expensesTypeDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Save new expensesTypeD");
		ExpensesType expensesType = mapper.map(expensesTypeDTO, ExpensesType.class);
		CommonFunctions.mapperToUpdate(expensesType, userClient, logger);

		// update & persist data in DB
		try {
			expensesType = expensesTypeRepository.save(expensesType);
		}
		catch (DataIntegrityViolationException ex) {
			throw new ExpensesTypeUnicityCodeException(
					new ExceptionResponseMessage(CommonErrorCode.UNIQUE_CODE_EXPENSES_TYPE,
							CommonExceptionsMessage.UNIQUE_CODE_EXPENSES_TYPE),
					CommonExceptionsMessage.UNIQUE_CODE_EXPENSES_TYPE);
		}

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ExpensesType.class.getSimpleName());
		return mapper.map(expensesType, ExpensesTypeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesTypeService#save(java.lang.Long,
	 * com.acm.utils.dtos.ExpensesTypeDTO)
	 */
	@Override
	public ExpensesTypeDTO save(Long id, ExpensesTypeDTO expensesTypeDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(expensesTypeDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update Expenses Type with ID = {}", id);
		ExpensesType oldExpensesType = expensesTypeRepository.findById(id).orElse(null);

		// check if object is null
		if (oldExpensesType == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ExpensesType.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ExpensesType.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// set new data
		oldExpensesType.setCode(expensesTypeDTO.getCode());
		oldExpensesType.setLibel(expensesTypeDTO.getLibel());
		oldExpensesType.setDescription(expensesTypeDTO.getDescription());
		oldExpensesType.setEnabled(expensesTypeDTO.getEnabled());
		oldExpensesType.setDocumentLibel(expensesTypeDTO.getDocumentLibel());
		oldExpensesType.setDocumentID(expensesTypeDTO.getDocumentID());

		// mapping new data with existing data (oldExpensesType)
		CommonFunctions.mapperToUpdate(oldExpensesType, userClient, logger);

		// update & persist data in DB
		ExpensesType newExpensesType = expensesTypeRepository.save(oldExpensesType);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ExpensesType.class.getSimpleName());
		return mapper.map(newExpensesType, ExpensesTypeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesTypeService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete Expenses Type with ID = {}", id);
		ExpensesType oldExpensesType = expensesTypeRepository.findById(id).orElse(null);

		// check if object is null
		if (oldExpensesType == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ExpensesType.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ExpensesType.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// delete old expenses type
		expensesTypeRepository.delete(oldExpensesType);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, ExpensesType.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesTypeService#findExpensesTypeById(java.lang.Long)
	 */
	@Override
	public ExpensesTypeDTO findExpensesTypeById(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find ExpensesType by ID : {}", id);
		ExpensesType expensesTypes = expensesTypeRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(expensesTypes)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ExpensesType.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ExpensesType.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(expensesTypes, ExpensesTypeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesTypeService#updateDocumentName(java.lang.String, java.lang.Long)
	 */
	@Override
	public void updateDocumentName(String documentLabel, Long documentId) {
		if (!ACMValidationUtils.isNullOrEmpty(documentId)) {
			expensesTypeRepository.updateDocumentName(documentLabel, documentId);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesTypeService#find(com.acm.utils.dtos.ExpensesTypeDTO)
	 */
	@Override
	public List<ExpensesTypeDTO> find(ExpensesTypeDTO expensesTypeDTO) {

		// init QExpensesType
		QExpensesType qExpensesType = QExpensesType.expensesType;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qExpensesType.enabled.eq(Boolean.TRUE));

		// QueryDSL using springDATA
		Iterable<ExpensesType> iterable = expensesTypeRepository.findAll(predicate);
		List<ExpensesType> expensestypes = new ArrayList<>();
		iterable.forEach(expensestypes::add);
		logger.info("{} : EXPENSES_TYPE was founded", expensestypes.size());

		// mapping returned list
		List<ExpensesTypeDTO> expensesTypeDTOs = new ArrayList<>();
		expensestypes.forEach(expensesType -> expensesTypeDTOs
				.add(mapper.map(expensesType, ExpensesTypeDTO.class)));

		logger.info("Returning founded data ...");
		return expensesTypeDTOs;
	}

}
