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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import com.acm.client.CreditClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.repository.AcmThirdPartyRepository;
import com.acm.service.AcmThirdPartyService;
import com.acm.utils.dtos.AcmThirdPartyDTO;
import com.acm.utils.dtos.pagination.AcmThirdPartyPaginationDTO;
import com.acm.utils.models.AcmThirdParty;
import com.acm.utils.models.QAcmThirdParty;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class AcmThirdPartyServiceImpl.
 */
@Service
public class AcmThirdPartyServiceImpl implements AcmThirdPartyService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmThirdPartyServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The acm third party repository. */
	@Autowired
	private AcmThirdPartyRepository acmThirdPartyRepository;

	@Autowired
	private CreditClient creditClient;

	/**
	 * Save.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the acm third party DTO
	 */
	@Override
	public AcmThirdPartyDTO save(AcmThirdPartyDTO acmThirdPartyDTO) {

		Preconditions.checkNotNull(acmThirdPartyDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// mapping & start inserting data
		AcmThirdParty thirdParty = mapper.map(acmThirdPartyDTO, AcmThirdParty.class);
		CommonFunctions.mapperToSave(thirdParty, null, logger);

		// insert new acmThirdParty
		AcmThirdParty NewacmThirdParty = acmThirdPartyRepository.save(thirdParty);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmThirdParty.class.getSimpleName());
		// save udfs
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getUserDefinedFieldsLinksDTOs())) {
			creditClient.updateUdfLinksByElementId(acmThirdPartyDTO.getUserDefinedFieldsLinksDTOs(),
					NewacmThirdParty.getId());
		}
		return mapper.map(NewacmThirdParty, AcmThirdPartyDTO.class);
	}

	/**
	 * Find.
	 *
	 * @param acmThirdPartyPaginationDTO the acm third party pagination DTO
	 * @return the acm third party pagination DTO
	 */
	@Override
	public AcmThirdPartyPaginationDTO find(AcmThirdPartyPaginationDTO acmThirdPartyPaginationDTO) {

		BooleanBuilder subPredicate = new BooleanBuilder();
		Preconditions.checkNotNull(acmThirdPartyPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getPageNumber())) {
			acmThirdPartyPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getPageSize())) {
			acmThirdPartyPaginationDTO.setPageSize(10);
		}
		// setting default data
		acmThirdPartyPaginationDTO.setResultsThirdParties(new ArrayList<>());
		// setting default totals pages
		acmThirdPartyPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		acmThirdPartyPaginationDTO.setTotalPages(0);
		// init QAcmThirdParty
		QAcmThirdParty qacmThirdParty = QAcmThirdParty.acmThirdParty;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find LIKE FirstName
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getFirstName())) {
			predicate.and(qacmThirdParty.firstName
					.like("%" + acmThirdPartyPaginationDTO.getParams().getFirstName() + "%"));
		}
		// find LIKE LastName
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getLastName())) {
			predicate.and(qacmThirdParty.lastName
					.like("%" + acmThirdPartyPaginationDTO.getParams().getLastName() + "%"));
		}
		// find LIKE Address
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getAddressParty())) {
			predicate.and(qacmThirdParty.addressParty
					.like("%" + acmThirdPartyPaginationDTO.getParams().getAddressParty() + "%"));
		}

		// find LIKE PhoneNumber
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getPhoneNumber())) {
			predicate.and(qacmThirdParty.phoneNumber
					.like("%" + acmThirdPartyPaginationDTO.getParams().getPhoneNumber() + "%"));
		}

		// find LIKE Mail
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getEmail())) {
			predicate.and(qacmThirdParty.email
					.like("%" + acmThirdPartyPaginationDTO.getParams().getEmail() + "%"));
		}

		// find LIKE type party
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getTypeParty())) {
			predicate.and(qacmThirdParty.typeParty
					.like("%" + acmThirdPartyPaginationDTO.getParams().getTypeParty() + "%"));
		}

		// find LIKE type in list
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getListTypes())) {
			predicate.and(
					qacmThirdParty.type.in(acmThirdPartyPaginationDTO.getParams().getListTypes()));
		}
		// find by enabled
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getEnabled())) {
			predicate.and(
					qacmThirdParty.enabled.eq(acmThirdPartyPaginationDTO.getParams().getEnabled()));
		}

		// find LIKE Id
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getId())) {
			predicate.and(qacmThirdParty.id
					.like("%" + acmThirdPartyPaginationDTO.getParams().getId() + "%"));
		}

		// find LIKE Type
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getType())) {
			predicate.and(qacmThirdParty.type
					.like("%" + acmThirdPartyPaginationDTO.getParams().getType() + "%"));
		}

		// find LIKE Statut
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getStatut())) {
			predicate.and(qacmThirdParty.statut
					.like("%" + acmThirdPartyPaginationDTO.getParams().getStatut() + "%"));
		}
		// find LIKE numero RNE
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getNumero_rne())) {
			predicate.and(qacmThirdParty.numero_rne
					.like("%" + acmThirdPartyPaginationDTO.getParams().getNumero_rne() + "%"));
		}
		// find LIKE code postal
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getCode_postal())) {
			predicate.and(qacmThirdParty.code_postal
					.like("%" + acmThirdPartyPaginationDTO.getParams().getCode_postal() + "%"));
		}
		// find LIKE pays
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getPays())) {
			predicate.and(qacmThirdParty.pays
					.like("%" + acmThirdPartyPaginationDTO.getParams().getPays() + "%"));
		}
		// find LIKE ville
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getVille())) {
			predicate.and(qacmThirdParty.ville
					.like("%" + acmThirdPartyPaginationDTO.getParams().getVille() + "%"));
		}
		// find LIKE type
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getType())) {
			predicate.and(qacmThirdParty.type
					.like("%" + acmThirdPartyPaginationDTO.getParams().getType() + "%"));
		}
		if (!ACMValidationUtils
				.isNullOrEmpty(acmThirdPartyPaginationDTO.getParams().getBranchID())) {

			subPredicate.or(qacmThirdParty.accessBranches
					.eq(acmThirdPartyPaginationDTO.getParams().getBranchID().toString()));

		}
		predicate.and(subPredicate);

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(acmThirdPartyPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getSortField())) {
			String sortedField = acmThirdPartyPaginationDTO.getSortField();
			if (acmThirdPartyPaginationDTO.getSortField().equals("firstName")) {
				sortedField = "firstName";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("lastName")) {
				sortedField = "lastName";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("email")) {
				sortedField = "email";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("addressParty")) {
				sortedField = "addressParty";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("typeParty")) {
				sortedField = "typeParty";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("phoneNumber")) {
				sortedField = "phoneNumber";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("enabled")) {
				sortedField = "enabled";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("code_postal")) {
				sortedField = "code_postal";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("statut")) {
				sortedField = "statut";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("numero_rne")) {
				sortedField = "numero_rne";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("type")) {
				sortedField = "type";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("ville")) {
				sortedField = "ville";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("pays")) {
				sortedField = "pays";
			}

			pageable = PageRequest.of(acmThirdPartyPaginationDTO.getPageNumber(),
					acmThirdPartyPaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
		}
		else if ("-1".equals(acmThirdPartyPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(acmThirdPartyPaginationDTO.getSortField())) {
			String sortedField = acmThirdPartyPaginationDTO.getSortField();
			if (acmThirdPartyPaginationDTO.getSortField().equals("firstName")) {
				sortedField = "firstName";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("lastName")) {
				sortedField = "lastName";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("email")) {
				sortedField = "email";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("addressParty")) {
				sortedField = "addressParty";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("typeParty")) {
				sortedField = "typeParty";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("phoneNumber")) {
				sortedField = "phoneNumber";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("branchID")) {
				sortedField = "branchID";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("branchName")) {
				sortedField = "branchName";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("branchDescription")) {
				sortedField = "branchDescription";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("accessBranches")) {
				sortedField = "accessBranches";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("enabled")) {
				sortedField = "enabled";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("code_postal")) {
				sortedField = "code_postal";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("statut")) {
				sortedField = "statut";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("numero_rne")) {
				sortedField = "numero_rne";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("type")) {
				sortedField = "type";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("ville")) {
				sortedField = "ville";
			}
			if (acmThirdPartyPaginationDTO.getSortField().equals("pays")) {
				sortedField = "pays";
			}

			pageable = PageRequest.of(acmThirdPartyPaginationDTO.getPageNumber(),
					acmThirdPartyPaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
		}
		else {
			// default sort by customerName : DESC
			pageable = PageRequest.of(acmThirdPartyPaginationDTO.getPageNumber(),
					acmThirdPartyPaginationDTO.getPageSize(), Sort.Direction.ASC, "dateInsertion");
		}
		// load data
		Page<AcmThirdParty> pagedResult = acmThirdPartyRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<AcmThirdParty> thirdParties = pagedResult.getContent();
			logger.info("{} : User was founded (PageNumber = {} / PageSize = {} )",
					thirdParties.size(), acmThirdPartyPaginationDTO.getPageNumber(),
					acmThirdPartyPaginationDTO.getPageSize());
			List<AcmThirdPartyDTO> thirdPartyDTOs = new ArrayList<>();
			thirdParties.forEach(thirdParty -> thirdPartyDTOs
					.add(mapper.map(thirdParty, AcmThirdPartyDTO.class)));
			// setting data
			acmThirdPartyPaginationDTO.setResultsThirdParties(thirdPartyDTOs);
			// setting totals pages
			acmThirdPartyPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			acmThirdPartyPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		logger.info("Executing Method UserPagination() :: DONE");
		return acmThirdPartyPaginationDTO;
	}

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the acm third party DTO
	 */
	@Override
	public AcmThirdPartyDTO save(Long id, AcmThirdPartyDTO acmThirdPartyDTO) {

		// Check acmThirdPartyDTO not null
		Preconditions.checkNotNull(acmThirdPartyDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Check id not null
		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Get Old setting third party by id
		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		AcmThirdParty oldThirdParty = acmThirdPartyRepository.findAcmThirdPartyById(id);
		oldThirdParty.setFirstName(acmThirdPartyDTO.getFirstName());
		oldThirdParty.setLastName(acmThirdPartyDTO.getLastName());
		oldThirdParty.setPhoneNumber(acmThirdPartyDTO.getPhoneNumber());
		oldThirdParty.setAddressParty(acmThirdPartyDTO.getAddressParty());
		oldThirdParty.setEmail(acmThirdPartyDTO.getEmail());
		oldThirdParty.setTypeParty(acmThirdPartyDTO.getTypeParty());
		oldThirdParty.setBranchID(acmThirdPartyDTO.getBranchID());
		oldThirdParty.setBranchName(acmThirdPartyDTO.getBranchName());
		oldThirdParty.setBranchDescription(acmThirdPartyDTO.getBranchDescription());
		oldThirdParty.setAccessBranches(acmThirdPartyDTO.getAccessBranches());
		oldThirdParty.setCode_postal(acmThirdPartyDTO.getCode_postal());
		oldThirdParty.setNumero_rne(acmThirdPartyDTO.getNumero_rne());
		oldThirdParty.setStatut(acmThirdPartyDTO.getStatut());
		oldThirdParty.setPays(acmThirdPartyDTO.getPays());
		oldThirdParty.setVille(acmThirdPartyDTO.getVille());
		oldThirdParty.setType(acmThirdPartyDTO.getType());
		CommonFunctions.mapperToUpdate(oldThirdParty, null, logger);

		// Update setting third party
		AcmThirdParty acmThirdParty = acmThirdPartyRepository.save(oldThirdParty);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmThirdParty.class.getSimpleName());
		// save udfs
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getUserDefinedFieldsLinksDTOs())) {
			creditClient.updateUdfLinksByElementId(acmThirdPartyDTO.getUserDefinedFieldsLinksDTOs(),
					acmThirdParty.getId());
		}
		return mapper.map(acmThirdParty, AcmThirdPartyDTO.class);
	}

	/**
	 * Save enable.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the acm third party DTO
	 */
	@Override
	public AcmThirdPartyDTO saveEnable(AcmThirdPartyDTO acmThirdPartyDTO) {

		// Check acmThirdPartyDTO not null
		Preconditions.checkNotNull(acmThirdPartyDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Check id third party not null
		Preconditions.checkNotNull(acmThirdPartyDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Get Old third party by id
		AcmThirdParty oldAcmThirdParty =
				acmThirdPartyRepository.findAcmThirdPartyById(acmThirdPartyDTO.getId());
		// Change third party enable
		oldAcmThirdParty.setEnabled(acmThirdPartyDTO.getEnabled());
		// Update third party
		AcmThirdParty thirdParty = acmThirdPartyRepository.save(oldAcmThirdParty);

		logger.info("Executing Method saveEnable() :: DONE");
		return mapper.map(thirdParty, AcmThirdPartyDTO.class);
	}

	/**
	 * Builds the query.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @param qAcmThirdParty the q acm third party
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(AcmThirdPartyDTO acmThirdPartyDTO,
			QAcmThirdParty qAcmThirdParty) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qAcmThirdParty.enabled.eq(Boolean.TRUE));
		// find only first Name
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getFirstName())) {
			predicate.and(qAcmThirdParty.firstName.like(acmThirdPartyDTO.getFirstName()));
		}

		// find only last Name
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getLastName())) {
			predicate.and(qAcmThirdParty.lastName.like(acmThirdPartyDTO.getLastName()));
		}
		// find only adress party
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getAddressParty())) {
			predicate.and(qAcmThirdParty.addressParty.like(acmThirdPartyDTO.getAddressParty()));
		}

		// find only email
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getEmail())) {
			predicate.and(qAcmThirdParty.email.like(acmThirdPartyDTO.getEmail()));
		}
		// find only phone number
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getPhoneNumber())) {
			predicate.and(qAcmThirdParty.phoneNumber.like(acmThirdPartyDTO.getPhoneNumber()));
		}

		// find only accessBranches
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getAccessBranches())) {
			predicate.and(qAcmThirdParty.accessBranches.like(acmThirdPartyDTO.getAccessBranches()));
		}
		// find only typeParty
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getTypeParty())) {
			predicate.and(qAcmThirdParty.typeParty.like(acmThirdPartyDTO.getTypeParty()));
		}
		// find only type
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getType())) {
			predicate.and(qAcmThirdParty.type.like(acmThirdPartyDTO.getType()));
		}
		// find only statut
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getStatut())) {
			predicate.and(qAcmThirdParty.statut.like(acmThirdPartyDTO.getStatut()));
		}
		// find only code postal
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getCode_postal())) {
			predicate.and(qAcmThirdParty.code_postal
					.like("%," + acmThirdPartyDTO.getCode_postal() + "%,"));
		}
		// find only rne
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getNumero_rne())) {
			predicate.and(
					qAcmThirdParty.numero_rne.like("%," + acmThirdPartyDTO.getNumero_rne() + "%,"));
		}
		// find only pays
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getPays())) {
			predicate.and(qAcmThirdParty.pays.like(acmThirdPartyDTO.getPays()));
		}
		// find only ville
		if (!ACMValidationUtils.isNullOrEmpty(acmThirdPartyDTO.getVille())) {
			predicate.and(qAcmThirdParty.ville.like(acmThirdPartyDTO.getVille()));
		}

		return predicate;
	}

	/**
	 * Find third party.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the list
	 */
	@Override
	public List<AcmThirdPartyDTO> findThirdParty(AcmThirdPartyDTO acmThirdPartyDTO) {

		// init QAcmThirdParty
		QAcmThirdParty qAcmThirdParty = QAcmThirdParty.acmThirdParty;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		predicate = buildQuery(acmThirdPartyDTO, qAcmThirdParty);
		// QueryDSL using springDATA
		Iterable<AcmThirdParty> iterable = acmThirdPartyRepository.findAll(predicate);
		List<AcmThirdParty> thirdParties = new ArrayList<>();
		iterable.forEach(thirdParties::add);
		logger.info("{} : ThirdParties was founded", thirdParties.size());

		// mapping returned list
		List<AcmThirdPartyDTO> amThirdPartyDTOs = new ArrayList<>();
		thirdParties.forEach(
				thirdParty -> amThirdPartyDTOs.add(mapper.map(thirdParty, AcmThirdPartyDTO.class)));

		return amThirdPartyDTOs;
	}

}
