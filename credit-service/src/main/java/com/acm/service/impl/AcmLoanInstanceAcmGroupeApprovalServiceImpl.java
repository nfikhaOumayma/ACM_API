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
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmLoanInstanceAcmGroupeApprovalRepository;
import com.acm.service.AcmLoanInstanceAcmGroupeApprovalService;
import com.acm.utils.dtos.AcmLoanInstanceAcmGroupeApprovalDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmLoanInstanceAcmGroupeApproval;
import com.acm.utils.models.Groupe;
import com.acm.utils.models.LoanInstance;
import com.acm.utils.models.QAcmLoanInstanceAcmGroupeApproval;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class AcmLoanInstanceGroupe_AssociationServiceImpl.
 */
@Service
public class AcmLoanInstanceAcmGroupeApprovalServiceImpl
		implements AcmLoanInstanceAcmGroupeApprovalService {

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The loan ins grp repo. */
	@Autowired
	private AcmLoanInstanceAcmGroupeApprovalRepository loanInsGrpRepo;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The repo. */
	@Autowired
	private AcmLoanInstanceAcmGroupeApprovalRepository acmLoanInstanceGroupeRepository;

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(AcmLoanInstanceAcmGroupeApprovalServiceImpl.class);

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmLoanInstanceGroupe_AssociationService#save(com.acm.utils.dtos.
	 * acmLoanInstanceGroupeAssociationDTO)
	 */
	@Override
	public AcmLoanInstanceAcmGroupeApprovalDTO save(
			AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO) {

		Preconditions.checkNotNull(acmLoanInstanceGroupeAssociationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmLoanInstanceAcmGroupeApproval loanInstanceGrp = mapper
				.map(acmLoanInstanceGroupeAssociationDTO, AcmLoanInstanceAcmGroupeApproval.class);

		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);

		AcmLoanInstanceAcmGroupeApproval newLoanInstanceGrp = loanInsGrpRepo.save(loanInstanceGrp);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				AcmLoanInstanceAcmGroupeApproval.class.getSimpleName());
		return mapper.map(newLoanInstanceGrp, AcmLoanInstanceAcmGroupeApprovalDTO.class);

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmLoanInstanceGroupe_AssociationService#save(java.lang.Long,
	 * com.acm.utils.dtos.acmLoanInstanceGroupeAssociationDTO)
	 */
	@Override
	public AcmLoanInstanceAcmGroupeApprovalDTO save(Long id,
			AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO)
			throws ResourcesNotFoundException {

		// Check acmLoanInstanceGroupeAssociationDTO not null
		Preconditions.checkNotNull(acmLoanInstanceGroupeAssociationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// Check id not null
		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Get Old setting collection third party by id
		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		AcmLoanInstanceAcmGroupeApproval oldAcmLoanInstanceGroupe =
				acmLoanInstanceGroupeRepository.findById(id).orElse(null);

		oldAcmLoanInstanceGroupe.setValidation(acmLoanInstanceGroupeAssociationDTO.getValidation());
		oldAcmLoanInstanceGroupe.setOwnerName(acmLoanInstanceGroupeAssociationDTO.getOwnerName());
		oldAcmLoanInstanceGroupe.setLoanInstance(mapper
				.map(acmLoanInstanceGroupeAssociationDTO.getLoanInstance(), LoanInstance.class));
		oldAcmLoanInstanceGroupe.setGroupe(
				mapper.map(acmLoanInstanceGroupeAssociationDTO.getGroupe(), Groupe.class));

		oldAcmLoanInstanceGroupe.setOwner(acmLoanInstanceGroupeAssociationDTO.getOwner());

		CommonFunctions.mapperToUpdate(oldAcmLoanInstanceGroupe, null, logger);

		// Update setting collection third party
		AcmLoanInstanceAcmGroupeApproval loanInsGp =
				acmLoanInstanceGroupeRepository.save(oldAcmLoanInstanceGroupe);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				AcmLoanInstanceAcmGroupeApproval.class.getSimpleName());

		return mapper.map(loanInsGp, AcmLoanInstanceAcmGroupeApprovalDTO.class);

	}

	/**
	 * Builds the query.
	 *
	 * @param acmLoanInstanceGroupeAssociationDTO the acm loan instance groupe association DTO
	 * @param qAcmLoanInstanceGroupeAssociation the q acm loan instance groupe association
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(
			AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO,
			QAcmLoanInstanceAcmGroupeApproval qAcmLoanInstanceGroupeAssociation) {

		BooleanBuilder predicate = new BooleanBuilder();

		// find by validation
		if (!ACMValidationUtils
				.isNullOrEmpty(acmLoanInstanceGroupeAssociationDTO.getValidation())) {
			predicate.and(qAcmLoanInstanceGroupeAssociation.validation
					.eq(acmLoanInstanceGroupeAssociationDTO.getValidation()));
		}

		// find only id loaninstance
		if (!ACMValidationUtils.isNullOrEmpty(acmLoanInstanceGroupeAssociationDTO.getLoanInstance())
				&& !ACMValidationUtils.isNullOrEmpty(
						acmLoanInstanceGroupeAssociationDTO.getLoanInstance().getId())) {
			predicate.and(qAcmLoanInstanceGroupeAssociation.loanInstance.id
					.eq(acmLoanInstanceGroupeAssociationDTO.getLoanInstance().getId()));
		}
		// find only groups
		if (!ACMValidationUtils.isNullOrEmpty(acmLoanInstanceGroupeAssociationDTO.getGroupe())
				&& !ACMValidationUtils
						.isNullOrEmpty(acmLoanInstanceGroupeAssociationDTO.getGroupe().getId())) {
			predicate.and(qAcmLoanInstanceGroupeAssociation.groupe.id
					.eq(acmLoanInstanceGroupeAssociationDTO.getGroupe().getId()));
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmLoanInstanceGroupeAssociationService#find(com.acm.utils.
	 * dtos.acmLoanInstanceGroupeAssociationDTO)
	 */
	@Override
	public List<AcmLoanInstanceAcmGroupeApprovalDTO> find(
			AcmLoanInstanceAcmGroupeApprovalDTO acmLoanInstanceGroupeAssociationDTO) {

		BooleanBuilder predicate = new BooleanBuilder();
		// init qAcmLoanInstanceGroupeAssociation
		QAcmLoanInstanceAcmGroupeApproval qAcmLoanInstanceGroupeAssociation =
				QAcmLoanInstanceAcmGroupeApproval.acmLoanInstanceAcmGroupeApproval;

		// init Predicate
		predicate =
				buildQuery(acmLoanInstanceGroupeAssociationDTO, qAcmLoanInstanceGroupeAssociation);

		// QueryDSL using springDATA
		/*
		 * Iterable<AcmLoanInstanceGroupe_Association> iterable = repo.findAll(predicate);
		 * List<AcmLoanInstanceGroupe_Association> insGrps = new ArrayList<>();
		 * iterable.forEach(insGrps::add); logger.info("{} : loanInstance Groups was founded",
		 * insGrps.size());
		 */

		// QueryDSL using springDATA
		Iterable<AcmLoanInstanceAcmGroupeApproval> iterable =
				acmLoanInstanceGroupeRepository.findAll(predicate);

		// iterable.forEach(insGrps::add);

		List<AcmLoanInstanceAcmGroupeApprovalDTO> acmLoanInstanceGroupeAssociationDTOs =
				new ArrayList<>();

		iterable.forEach(loanGrp -> acmLoanInstanceGroupeAssociationDTOs
				.add(mapper.map(loanGrp, AcmLoanInstanceAcmGroupeApprovalDTO.class)));

		logger.info("{} : AcmLoanInstanceAcmGroupeApproval was founded",
				acmLoanInstanceGroupeAssociationDTOs.size());

		return acmLoanInstanceGroupeAssociationDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmLoanInstanceGroupeAssociationService#updateAll(java.util.List)
	 */
	@Override
	public void updateAll(
			List<AcmLoanInstanceAcmGroupeApprovalDTO> acmLoanInstanceGroupeAssociationDTOs) {

		List<AcmLoanInstanceAcmGroupeApproval> loanInstanceGroupeAssociation = new ArrayList<>();
		acmLoanInstanceGroupeAssociationDTOs.forEach(item -> {
			Preconditions.checkNotNull(item, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
			AcmLoanInstanceAcmGroupeApproval acmLoanInstanceGroupeAssociation =
					mapper.map(item, AcmLoanInstanceAcmGroupeApproval.class);
			CommonFunctions.mapperToSave(acmLoanInstanceGroupeAssociation, userClient, logger);
			loanInstanceGroupeAssociation.add(acmLoanInstanceGroupeAssociation);

		});
		acmLoanInstanceGroupeRepository.saveAll(loanInstanceGroupeAssociation);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				AcmLoanInstanceAcmGroupeApproval.class.getSimpleName());

	}

}
