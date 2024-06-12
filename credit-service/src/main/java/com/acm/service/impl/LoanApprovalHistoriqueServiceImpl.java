/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.LoanApprovalHistoriqueRepository;
import com.acm.service.LoanApprovalHistoriqueService;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;
import com.acm.utils.models.Loan;
import com.acm.utils.models.LoanApprovalHistorique;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link LoanApprovalHistoriqueServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
@Service
public class LoanApprovalHistoriqueServiceImpl implements LoanApprovalHistoriqueService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(LoanApprovalHistoriqueServiceImpl.class);

	/** The loanApprovalHistorique repository. */
	@Autowired
	private LoanApprovalHistoriqueRepository loanApprovalHistoriqueRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanApprovalHistoriqueService#find(java.lang.Integer)
	 */
	@Override
	public LoanApprovalHistoriqueDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find LoanApprovalHistorique by ID : {}", id);
		LoanApprovalHistorique loanApprovalHistorique =
				loanApprovalHistoriqueRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(loanApprovalHistorique)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					LoanApprovalHistorique.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ LoanApprovalHistorique.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(loanApprovalHistorique, LoanApprovalHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanApprovalHistoriqueService#find(com.acm.utils.dtos.
	 * LoanApprovalHistoriqueDTO)
	 */
	@Override
	public List<LoanApprovalHistoriqueDTO> find(
			LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO) {

		List<LoanApprovalHistoriqueDTO> loanApprovalHistoriqueDTOs = new ArrayList<>();
		if (loanApprovalHistoriqueDTO != null && loanApprovalHistoriqueDTO.getLoanDTO() != null
				&& loanApprovalHistoriqueDTO.getLoanDTO().getLoanId() != null) {
			// load data from DB
			List<LoanApprovalHistorique> loanApprovalHistoriques =
					loanApprovalHistoriqueRepository.findByLoanAndEnabledOrderByApprovalDateDesc(
							new Loan(loanApprovalHistoriqueDTO.getLoanDTO().getLoanId()),
							Boolean.TRUE);
			// mapping founded data
			loanApprovalHistoriques.forEach(loanApprovalHistorique -> loanApprovalHistoriqueDTOs
					.add(mapper.map(loanApprovalHistorique, LoanApprovalHistoriqueDTO.class)));
		}
		logger.info("Returning founded data");
		return loanApprovalHistoriqueDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanApprovalHistoriqueService#save(com.acm.utils.dtos.
	 * LoanApprovalHistoriqueDTO)
	 */
	@Override
	public LoanApprovalHistoriqueDTO saveAndSetApprovalLabel(
			LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO) {

		Preconditions.checkNotNull(loanApprovalHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		LoanApprovalHistorique loanApprovalHistorique =
				mapper.map(loanApprovalHistoriqueDTO, LoanApprovalHistorique.class);
		loanApprovalHistorique.setApprovalDate(new Date());
		loanApprovalHistorique
				.setApprovedBy(CommonFunctions.getConnectedUser(logger).getFullName());

		// setting status : ApprovalDesicionLabel
		AcmStatutsDTO acmStatutsApprovalDesicionLabel = CommonFunctions.loadStatus(
				loanApprovalHistorique.getApprovalDesicion(),
				Arrays.asList(
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.UPDATE_ASSIGNED_TO_CUSTOMER),
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.LOAN_REASSIGN),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_CANCELLED),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_APPROVED),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REJECTED),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REVIEW),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.LOAN_RISK_AUDIT_STATUS_RECOMMEND),
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.LOAN_REVERSER)));
		loanApprovalHistorique.setApprovalDesicionLabel(
				acmStatutsApprovalDesicionLabel != null ? acmStatutsApprovalDesicionLabel.getValue()
						: "");

		// setting status : ApprovalLevelLabel
		String approvalLevelLabel = loanApprovalHistorique.getApprovalLevelLabel();
		AcmStatutsDTO acmStatutsApprovalLevelLabel = CommonFunctions.loadStatus(
				loanApprovalHistorique.getApprovalLevel(),
				Arrays.asList(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1),
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2),
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L3),
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L4),
						CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT),
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.RISK),
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT)));
		// loanApprovalHistorique.setApprovalLevelLabel(
		// acmStatutsApprovalLevelLabel != null ? acmStatutsApprovalLevelLabel.getValue()
		// : "");

		loanApprovalHistorique.setApprovalLevelLabel(
				acmStatutsApprovalLevelLabel != null ? acmStatutsApprovalLevelLabel.getValue()
						: approvalLevelLabel != null ? approvalLevelLabel : "");

		CommonFunctions.mapperToSave(loanApprovalHistorique, userClient, logger);
		// persist data in DB
		LoanApprovalHistorique newDocumentsLoan =
				loanApprovalHistoriqueRepository.save(loanApprovalHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				LoanApprovalHistorique.class.getSimpleName());
		return mapper.map(newDocumentsLoan, LoanApprovalHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanApprovalHistoriqueService#save(com.acm.utils.dtos.
	 * LoanApprovalHistoriqueDTO)
	 */
	@Override
	public LoanApprovalHistoriqueDTO save(LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO) {

		Preconditions.checkNotNull(loanApprovalHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		LoanApprovalHistorique loanApprovalHistorique =
				mapper.map(loanApprovalHistoriqueDTO, LoanApprovalHistorique.class);
		loanApprovalHistorique.setApprovalDate(new Date());
		loanApprovalHistorique
				.setApprovedBy(CommonFunctions.getConnectedUser(logger).getFullName());

		CommonFunctions.mapperToSave(loanApprovalHistorique, userClient, logger);
		// persist data in DB
		LoanApprovalHistorique newDocumentsLoan =
				loanApprovalHistoriqueRepository.save(loanApprovalHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				LoanApprovalHistorique.class.getSimpleName());
		return mapper.map(newDocumentsLoan, LoanApprovalHistoriqueDTO.class);
	}
}
