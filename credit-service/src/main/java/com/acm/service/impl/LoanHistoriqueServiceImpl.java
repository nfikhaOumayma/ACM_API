/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.LoanHistoriqueRepository;
import com.acm.service.CustomerDecisionService;
import com.acm.service.LoanApprovalHistoriqueService;
import com.acm.service.LoanHistoriqueService;
import com.acm.utils.dtos.CustomerDecisionDTO;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanHistoriqueDTO;
import com.acm.utils.dtos.LoanNoteHistoriqueDTO;
import com.acm.utils.models.Loan;
import com.acm.utils.models.LoanHistorique;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link LoanHistoriqueServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Service
public class LoanHistoriqueServiceImpl implements LoanHistoriqueService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanHistoriqueServiceImpl.class);

	/** The loanHistorique repository. */
	@Autowired
	private LoanHistoriqueRepository loanHistoriqueRepository;

	/** The CustomerDesicion service. */
	@Autowired
	private CustomerDecisionService customerDesicionService;

	/** The LoanApprovalHistorique service. */
	@Autowired
	private LoanApprovalHistoriqueService loanApprovalHistoriqueService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanHistoriqueService#find(java.lang.Integer)
	 */
	@Override
	public LoanHistoriqueDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find LoanHistorique by ID : {}", id);
		LoanHistorique loanHistorique = loanHistoriqueRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(loanHistorique)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					LoanHistorique.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ LoanHistorique.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		return mapper.map(loanHistorique, LoanHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanHistoriqueService#find(com.acm.utils.dtos.LoanHistoriqueDTO)
	 */
	@Override
	public List<LoanHistoriqueDTO> find(LoanHistoriqueDTO loanHistoriqueDTO) {

		List<LoanHistoriqueDTO> loanHistoriqueDTOs = new ArrayList<>();
		if (loanHistoriqueDTO != null && loanHistoriqueDTO.getLoanDTO() != null
				&& loanHistoriqueDTO.getLoanDTO().getLoanId() != null) {
			List<LoanHistorique> loanHistoriques =
					loanHistoriqueRepository.findByLoanAndTechniqueInformationOrderByDateUpdateDesc(
							new Loan(loanHistoriqueDTO.getLoanDTO().getLoanId()), Boolean.FALSE);
			loanHistoriques.forEach(loanHistorique -> loanHistoriqueDTOs
					.add(mapper.map(loanHistorique, LoanHistoriqueDTO.class)));
		}
		// filter returned data
		// Set<Object> loanHistoriqueSet = new HashSet<>();
		// removing duplicated elements from loanHistoriqueDTOs if already existed in set
		// loanHistoriqueDTOs.removeIf(lh -> !loanHistoriqueSet.add(lh.getDescription()));
		logger.info("Returning founded data");
		return loanHistoriqueDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanHistoriqueService#save(com.acm.utils.dtos.LoanHistoriqueDTO)
	 */
	@Override
	public LoanHistoriqueDTO save(LoanHistoriqueDTO loanHistoriqueDTO) {

		Preconditions.checkNotNull(loanHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		LoanHistorique loanHistorique = mapper.map(loanHistoriqueDTO, LoanHistorique.class);
		loanHistorique.setDateUpdate(new Date());
		LoanHistorique newDocumentsLoan = loanHistoriqueRepository.save(loanHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, LoanHistorique.class.getSimpleName());
		return mapper.map(newDocumentsLoan, LoanHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanHistoriqueService#save(com.acm.utils.dtos.LoanHistoriqueDTO,
	 * java.lang.String)
	 */
	@Override
	public LoanHistoriqueDTO save(LoanHistoriqueDTO loanHistoriqueDTO, String insertBy) {

		Preconditions.checkNotNull(loanHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		LoanHistorique loanHistorique = mapper.map(loanHistoriqueDTO, LoanHistorique.class);
		loanHistorique.setDateUpdate(new Date());
		loanHistorique.setUpdatedBy(insertBy);
		LoanHistorique newDocumentsLoan = loanHistoriqueRepository.save(loanHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, LoanHistorique.class.getSimpleName());
		return mapper.map(newDocumentsLoan, LoanHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanNoteHistoriqueService#find(com.acm.utils.dtos.LoanNoteHistoriqueDTO)
	 */
	@Override
	public List<LoanNoteHistoriqueDTO> find(LoanNoteHistoriqueDTO loanNoteHistoriqueDTO) {

		// Find {@link List} of {@link LoanApprovalHistoriqueDTO} by given params.
		List<LoanApprovalHistoriqueDTO> loanApprovalHistoriqueDTOs = loanApprovalHistoriqueService
				.find(new LoanApprovalHistoriqueDTO(loanNoteHistoriqueDTO.getLoanDTO()));
		// Find {@link List} of {@link CustomerDecisionDTO} by given params.
		List<CustomerDecisionDTO> customerDecisionDTOs = customerDesicionService
				.find(new CustomerDecisionDTO(loanNoteHistoriqueDTO.getLoanDTO().getLoanId()));

		List<LoanNoteHistoriqueDTO> loanNoteHistoriqueDTOs = new ArrayList<>();
		// mapping founded Loan Approval Historique data
		loanApprovalHistoriqueDTOs.forEach(loanApprovalHistoriqueDTO -> loanNoteHistoriqueDTOs
				.add(new LoanNoteHistoriqueDTO(loanApprovalHistoriqueDTO.getLoanDTO(),
						loanApprovalHistoriqueDTO.getApprovalDate(),
						loanApprovalHistoriqueDTO.getApprovalAmount(),
						loanApprovalHistoriqueDTO.getApprovalDesicion(),
						loanApprovalHistoriqueDTO.getApprovalDesicionLabel(),
						loanApprovalHistoriqueDTO.getApprovalNote(),
						loanApprovalHistoriqueDTO.getApprovalLevel(),
						loanApprovalHistoriqueDTO.getApprovalLevelLabel(),
						loanApprovalHistoriqueDTO.getApprovedBy(), "APPROVEL")));
		// mapping founded Customer Decision data
		customerDecisionDTOs.forEach(customerDecisionDTO -> loanNoteHistoriqueDTOs
				.add(new LoanNoteHistoriqueDTO(new LoanDTO(customerDecisionDTO.getIdLoan()),
						customerDecisionDTO.getContactDate(), customerDecisionDTO.getComments(),
						customerDecisionDTO.getStatusId(), customerDecisionDTO.getStatusLibelle(),
						customerDecisionDTO.getAmount(), customerDecisionDTO.getInsertBy(),
						"NOTE")));
		// filter by action date DESC
		loanNoteHistoriqueDTOs
				.sort(Comparator.comparing(LoanNoteHistoriqueDTO::getActionDate).reversed());
		logger.info("Returning founded data");
		return loanNoteHistoriqueDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanHistoriqueService#saveForTimer(com.acm.utils.dtos.
	 * LoanHistoriqueDTO)
	 */
	@Override
	public LoanHistoriqueDTO saveForTimer(LoanDTO loanDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// find by id
		LoanHistorique oldLoan =
				loanHistoriqueRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (oldLoan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
		}
		// setting category
		oldLoan.setCategory(loanDTO.getCategory());
		LoanHistorique newLoan = loanHistoriqueRepository.save(oldLoan);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
		return mapper.map(newLoan, LoanHistoriqueDTO.class);
	}
}
