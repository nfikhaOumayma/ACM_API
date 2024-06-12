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
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.LoanInstanceRepository;
import com.acm.service.LoanInstanceService;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanInstanceDTO;
import com.acm.utils.dtos.SettingStatutWorkflowDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.Loan;
import com.acm.utils.models.LoanInstance;
import com.acm.utils.models.QLoanInstance;
import com.acm.utils.models.SettingStatutWorkflow;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link LoanInstanceServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
@Service
public class LoanInstanceServiceImpl implements LoanInstanceService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanInstanceServiceImpl.class);

	/** The loanInstance repository. */
	@Autowired
	private LoanInstanceRepository loanInstanceRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanInstanceService#find(com.acm.utils.dtos. LoanInstanceDTO)
	 */
	@Override
	public List<LoanInstanceDTO> find(LoanInstanceDTO loanInstanceDTO) {

		Preconditions.checkNotNull(loanInstanceDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		if (ACMValidationUtils.isNullOrEmpty(loanInstanceDTO.getIdLoan())
				&& ACMValidationUtils.isNullOrEmpty(loanInstanceDTO.getId())
				&& ACMValidationUtils.isNullOrEmpty(loanInstanceDTO.getCode())) {
			return new ArrayList<>();
		}
		// init QLoanInstance
		QLoanInstance qLoanInstance = QLoanInstance.loanInstance;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// only enabled row
		predicate.and(qLoanInstance.enabled.eq(Boolean.TRUE));

		// find by loan
		if (!ACMValidationUtils.isNullOrEmpty(loanInstanceDTO.getIdLoan())) {
			predicate.and(qLoanInstance.loan.eq(new Loan(loanInstanceDTO.getIdLoan())));
		}
		// find by id loan instance
		if (!ACMValidationUtils.isNullOrEmpty(loanInstanceDTO.getId())) {
			predicate.and(qLoanInstance.id.eq(loanInstanceDTO.getId()));
		}
		// find by code
		if (!ACMValidationUtils.isNullOrEmpty(loanInstanceDTO.getCode())) {
			predicate.and(qLoanInstance.code.eq(loanInstanceDTO.getCode()));
		}
		// QueryDSL using springDATA
		Iterable<LoanInstance> iterable = loanInstanceRepository.findAll(predicate,
				Sort.by(Direction.ASC, "orderEtapeProcess"));
		List<LoanInstance> loanInstances = new ArrayList<>();
		iterable.forEach(loanInstances::add);

		// mapping returned list
		List<LoanInstanceDTO> loanInstanceDTOs = new ArrayList<>();
		loanInstances.forEach(loanInstance -> loanInstanceDTOs
				.add(mapper.map(loanInstance, LoanInstanceDTO.class)));

		logger.info("Returning founded data");
		return loanInstanceDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanInstanceService#save(com.acm.utils.dtos.LoanInstanceDTO)
	 */
	@Override
	public LoanInstanceDTO save(LoanInstanceDTO loanInstanceDTO) {

		// TODO loan process
		Preconditions.checkNotNull(loanInstanceDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		LoanInstance loanInstance = mapper.map(loanInstanceDTO, LoanInstance.class);
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		loanInstance.setInsertBy(userDTO.getFullName());
		loanInstance.setAcmVersion(0);
		loanInstance.setDateInsertion(new Date());
		LoanInstance newLoanInstance = loanInstanceRepository.save(loanInstance);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, LoanInstance.class.getSimpleName());
		return mapper.map(newLoanInstance, LoanInstanceDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanInstanceService#save(java.lang.Long,
	 * com.acm.utils.dtos.LoanInstanceDTO)
	 */
	@Override
	public LoanInstanceDTO save(Long id, LoanInstanceDTO loanInstanceDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(loanInstanceDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update LoanInstance with ID = {}", id);
		LoanInstance oldLoanInstance = loanInstanceRepository.findById(id).orElse(null);

		// check if object is null
		if (oldLoanInstance == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, LoanInstance.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + LoanInstance.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldLoanInstance)
		mapper.map(loanInstanceDTO, oldLoanInstance);
		CommonFunctions.mapperToUpdate(oldLoanInstance, userClient, logger);

		// update & persist data in DB
		LoanInstance newLoanInstance = loanInstanceRepository.save(oldLoanInstance);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, LoanInstance.class.getSimpleName());
		return mapper.map(newLoanInstance, LoanInstanceDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanInstanceService#updateForWorkflow(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public List<LoanInstanceDTO> updateForWorkflow(LoanDTO loanDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// Delete existing loan process data
		Long numberOfDeletedData =
				loanInstanceRepository.deleteByLoan(new Loan(loanDTO.getLoanId()));
		logger.info(" number Of Deleted Data = {}", numberOfDeletedData);

		// Find workflow process
		List<SettingStatutWorkflowDTO> settingStatutWorkflowDTOs = parametrageClient
				.findLoanProcess(new SettingStatutWorkflowDTO(loanDTO, CommonConstants.APP_CLIENT,
						ACMConstantWorkflowStatuts.PROCESS_VALIDATION_LOAN_TAMKEEN));
		if (ACMValidationUtils.isNullOrEmpty(settingStatutWorkflowDTOs)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingStatutWorkflow.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingStatutWorkflow.class.getSimpleName()
							+ "for process name : "
							+ ACMConstantWorkflowStatuts.PROCESS_VALIDATION_LOAN_TAMKEEN
							+ " and for client : " + CommonConstants.APP_CLIENT);
		}
		List<LoanInstanceDTO> newLoanInstanceDTOs = new ArrayList<>();
		// Save process loan
		settingStatutWorkflowDTOs.forEach(settingStatutWorkflowDTO -> {
			LoanInstanceDTO newLoanInstanceDTO = save(new LoanInstanceDTO(loanDTO.getLoanId(),
					settingStatutWorkflowDTO.getCode(), settingStatutWorkflowDTO.getLibelle(),
					settingStatutWorkflowDTO.getDescription(),
					settingStatutWorkflowDTO.getCodeStatutLoan(),
					settingStatutWorkflowDTO.getStatutLoan(), settingStatutWorkflowDTO.getIhmRoot(),
					settingStatutWorkflowDTO.getClient(), settingStatutWorkflowDTO.getProcessName(),
					settingStatutWorkflowDTO.getOrderEtapeProcess(),
					settingStatutWorkflowDTO.getShowIb(), settingStatutWorkflowDTO.getCodeStautIb(),
					settingStatutWorkflowDTO.getEnabled(), null));
			logger.debug("{}", newLoanInstanceDTO);
			newLoanInstanceDTOs.add(newLoanInstanceDTO);
			logger.info(
					"Loan process for loan with AccountNumber = [{}] was successfully inserted :: DONE",
					loanDTO.getAccountNumber());
		});
		loanDTO.setLoanInstancesDtos(newLoanInstanceDTOs);
		return newLoanInstanceDTOs;
	}
}
