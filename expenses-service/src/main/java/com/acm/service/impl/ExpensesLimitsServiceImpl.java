/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.ReportingClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ExpenseDrAndCrAccountsEmptyException;
import com.acm.exceptions.type.ExpensesLimitNotFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ExpensesLimitsRepository;
import com.acm.service.ExpensesLimitsService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.ExpensesLimitDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.models.ExpensesLimit;
import com.acm.utils.models.QExpensesLimit;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link ExpensesLimitsServiceImpl} class.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
@Service
public class ExpensesLimitsServiceImpl implements ExpensesLimitsService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(ExpensesLimitsServiceImpl.class);

	/** The expenses limits repository. */
	@Autowired
	private ExpensesLimitsRepository expensesLimitsRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesLimitsService#find(com.acm.utils.dtos.ExpensesLimitDTO)
	 */
	@Override
	public List<ExpensesLimitDTO> find(ExpensesLimitDTO expensesLimitDTO) {

		logger.info("Start Method = find()");
		Preconditions.checkNotNull(expensesLimitDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QExpensesLimit
		QExpensesLimit qExpensesLimit = QExpensesLimit.expensesLimit;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find by branch
		if (!ACMValidationUtils.isNullOrEmpty(expensesLimitDTO.getIdBranch())) {
			predicate.and(qExpensesLimit.idBranch.eq(expensesLimitDTO.getIdBranch()));
		}
		// find by Expenses Type
		if (!ACMValidationUtils.isNullOrEmpty(expensesLimitDTO.getIdExpensesType())) {
			predicate.and(qExpensesLimit.idExpensesType.eq(expensesLimitDTO.getIdExpensesType()));
		}
		// find by limit
		if (!ACMValidationUtils.isNullOrEmpty(expensesLimitDTO.getLimit())) {
			predicate.and(qExpensesLimit.limit.eq(expensesLimitDTO.getLimit()));
		}

		// Get result
		Iterable<ExpensesLimit> iterable = expensesLimitsRepository.findAll(predicate);

		// create Response
		List<ExpensesLimit> expensesLimits = new ArrayList<>();
		iterable.forEach(expensesLimits::add);
		logger.info("Method = find() :{} : Expenses Limit was founded", expensesLimits.size());

		List<ExpensesLimitDTO> expensesLimitDTOs = new ArrayList<>();
		expensesLimits.forEach(expensesLimit -> expensesLimitDTOs
				.add(mapper.map(expensesLimit, ExpensesLimitDTO.class)));
		return expensesLimitDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesLimitsService#save(java.util.List)
	 */
	@Override
	public List<ExpensesLimitDTO> save(List<ExpensesLimitDTO> expensesLimitDTOs) {

		logger.info("Start Method = save()");
		Preconditions.checkNotNull(expensesLimitDTOs, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// delete old configuration
		expensesLimitsRepository.deleteByIdBranch(expensesLimitDTOs.get(0).getIdBranch());

		// List for response
		List<ExpensesLimitDTO> newExpensesLimitDTOs = new ArrayList<>();

		// save new configuration
		expensesLimitDTOs.forEach(expensesLimitDTO -> {
			ExpensesLimit expensesLimit = mapper.map(expensesLimitDTO, ExpensesLimit.class);
			CommonFunctions.mapperToSave(expensesLimit, userClient, logger);
			ExpensesLimit newExpensesLimit = expensesLimitsRepository.save(expensesLimit);
			newExpensesLimitDTOs.add(mapper.map(newExpensesLimit, ExpensesLimitDTO.class));
		});
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, ExpensesLimit.class.getSimpleName());
		return newExpensesLimitDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesLimitsService#refrechLimits()
	 */
	@Override
	public Date refreshLimits(String lasUpdateDateKey) {

		logger.info("Start Method = refrechLimits()");
		// find all Expenses limits
		List<ExpensesLimit> expensesLimits = this.expensesLimitsRepository.findAll();
		// list for return
		List<ExpensesLimitDTO> expensesLimitDTOs = new ArrayList<>();
		// update Expenses limits
		expensesLimits.forEach(expensesLimit -> {
			logger.info("Update Expenses Limit with ID = {}", expensesLimit.getId());
			expensesLimit.setRestLimit(expensesLimit.getLimit());
			expensesLimitDTOs.add(mapper.map(this.expensesLimitsRepository.save(expensesLimit),
					ExpensesLimitDTO.class));
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ExpensesLimit.class.getSimpleName());
		});
		// get : DATE_LAST_UPDATE_REFRESH_LIMIT_EXPENSES from acmEnvironnement
		AcmEnvironnementDTO acmEnvironmentDTO = this.parametrageClient.find(lasUpdateDateKey);
		// send mail and update dateLastUpdate
		if (!ACMValidationUtils.isNullOrEmpty(expensesLimitDTOs)) {
			Integer count = expensesLimitDTOs.size();
			// send mail

			mailSenderClient.sendMail(new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
					defaultACMReceiverMail, "Processing Expenses",
					"Processing Expenses has been successfully run, [" + count
							+ "] expenses are updated in ACM-DB"));
			logger.debug("Sending Email for Processing Expenses refresh :: DONE");
			// set new date last update
			acmEnvironmentDTO.setDateLastUpdate(new Date());
			// set acm_environnement_value
			acmEnvironmentDTO.setValue((new Date()).toString());
			// update date
			AcmEnvironnementDTO updatedAcmEnvironmentDTO =
					this.parametrageClient.updateAcmEnvironnementDTO(acmEnvironmentDTO);
			// return updated date
			return updatedAcmEnvironmentDTO.getDateLastUpdate();
		}

		return acmEnvironmentDTO.getDateLastUpdate();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesLimitService#save(com.acm.utils.dtos.ExpensesLimitDTO)
	 */
	@Override
	public ExpensesLimitDTO save(ExpensesLimitDTO expensesLimitDTO) {

		Preconditions.checkNotNull(expensesLimitDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		ExpensesLimit expensesLimit = mapper.map(expensesLimitDTO, ExpensesLimit.class);
		CommonFunctions.mapperToSave(expensesLimit, userClient, logger);
		ExpensesLimit newExpensesLimit = expensesLimitsRepository.save(expensesLimit);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, ExpensesLimit.class.getSimpleName());
		return mapper.map(newExpensesLimit, ExpensesLimitDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesLimitsService#save(java.lang.Long,
	 * com.acm.utils.dtos.ExpensesLimitDTO)
	 */
	@Override
	public ExpensesLimitDTO save(Long id, ExpensesLimitDTO expensesLimitDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(expensesLimitDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update Expenses Limite with ID = {}", id);
		ExpensesLimit oldExpensesLimit = expensesLimitsRepository.findById(id).orElse(null);

		// check if object is null
		if (oldExpensesLimit == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ExpensesLimit.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ExpensesLimit.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		mapper.map(expensesLimitDTO, oldExpensesLimit);
		CommonFunctions.mapperToUpdate(oldExpensesLimit, userClient, logger);
		ExpensesLimit newExpensesLimit = expensesLimitsRepository.save(oldExpensesLimit);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ExpensesLimit.class.getSimpleName());

		return mapper.map(newExpensesLimit, ExpensesLimitDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesLimitsService#findByTypeAndBranchId(com.acm.utils.dtos.
	 * ExpensesLimitDTO)
	 */
	@Override
	public ExpensesLimitDTO findByTypeAndBranchId(ExpensesLimitDTO expensesLimitDTO)
			throws ExpenseDrAndCrAccountsEmptyException, ExpensesLimitNotFoundException {

		Preconditions.checkNotNull(expensesLimitDTO.getIdExpensesType(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(expensesLimitDTO.getIdBranch(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		ExpensesLimit expensesLimit = expensesLimitsRepository.findByIdExpensesTypeAndIdBranch(
				expensesLimitDTO.getIdExpensesType(), expensesLimitDTO.getIdBranch());

		if (ACMValidationUtils.isNullOrEmpty(expensesLimit)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ExpensesLimit.class.getSimpleName());
			throw new ExpensesLimitNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ExpensesLimit.class.getSimpleName());
		}
		else if (ACMValidationUtils.isNullOrEmpty(expensesLimit.getCr())
				|| ACMValidationUtils.isNullOrEmpty(expensesLimit.getDr())) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ExpensesLimit.class.getSimpleName());
			throw new ExpenseDrAndCrAccountsEmptyException(
					CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.CR_OR_DR_ACCOUNTS_ARE_EMPTY
							+ ExpensesLimit.class.getSimpleName());
		}

		return mapper.map(expensesLimit, ExpensesLimitDTO.class);
	}
}
