/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.CustomerLinksRelationshipRepository;
import com.acm.service.CustomerLinksRelationshipService;
import com.acm.service.CustomerService;
import com.acm.service.LoanService;
import com.acm.service.api_ib.LoadDataIBService;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.LinkRelationshipsCategory;
import com.acm.utils.models.Customer;
import com.acm.utils.models.CustomerLinksRelationship;
import com.acm.utils.models.Loan;
import com.acm.utils.models.QCustomerLinksRelationship;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link CustomerLinksRelationshipServiceImpl} CustomerLinksRelationshipServiceImpl.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
@Service
public class CustomerLinksRelationshipServiceImpl implements CustomerLinksRelationshipService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(CustomerLinksRelationshipServiceImpl.class);

	/** The customerLinksRelationship repository. */
	@Autowired
	private CustomerLinksRelationshipRepository customerLinksRelationshipRepository;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The load data IB service. */
	@Autowired
	private LoadDataIBService loadDataIBService;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerLinksRelationshipService#find(com.acm.utils.dtos.
	 * CustomerLinksRelationshipDTO)
	 */
	@Override
	public List<CustomerLinksRelationshipDTO> find(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		Preconditions.checkNotNull(customerLinksRelationshipDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// Check Loan Id and Customer Id
		if (ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getCustomerId())
				&& ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getIdLoan())
				&& Boolean.FALSE.equals(customerLinksRelationshipDTO.getForceSearch())
				&& ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getLoanIds())) {
			return new ArrayList<>();
		}

		// init QCustomerLinksRelationship
		QCustomerLinksRelationship qCustomerLinksRelationship =
				QCustomerLinksRelationship.customerLinksRelationship;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qCustomerLinksRelationship.enabled.eq(Boolean.TRUE));

		// find loan guarantors of list of loans in parameter :loanIds
		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getLoanIds())) {
			predicate.and(qCustomerLinksRelationship.idLoan
					.in(customerLinksRelationshipDTO.getLoanIds()));
		}
		// find by customerId
		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getCustomerId())) {
			predicate.and(qCustomerLinksRelationship.customerId
					.eq(customerLinksRelationshipDTO.getCustomerId()));
		}
		// find by id member
		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getMember())
				&& !ACMValidationUtils
						.isNullOrEmpty(customerLinksRelationshipDTO.getMember().getId())) {
			predicate.and(qCustomerLinksRelationship.member
					.eq(new Customer(customerLinksRelationshipDTO.getMember().getId())));
		}
		// find by id Loan
		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getIdLoan())) {
			predicate.and(
					qCustomerLinksRelationship.idLoan.eq(customerLinksRelationshipDTO.getIdLoan()));
		}
		// find by id Category
		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getCategory())) {
			predicate.and(qCustomerLinksRelationship.category
					.eq(customerLinksRelationshipDTO.getCategory()));
		}
		// test for group members
		if (Boolean.TRUE.equals(customerLinksRelationshipDTO.getCheckForGroup())) {
			predicate.and(qCustomerLinksRelationship.linkRelationshipType.ne("Partner"));
		}
		Iterable<CustomerLinksRelationship> iterable =
				customerLinksRelationshipRepository.findAll(predicate);
		List<CustomerLinksRelationship> customerLinksRelationships = new ArrayList<>();
		iterable.forEach(customerLinksRelationships::add);
		logger.info("{} : CustomerLinksRelationship was founded",
				customerLinksRelationships.size());

		List<CustomerLinksRelationshipDTO> customerLinksRelationshipsDTOs = new ArrayList<>();
		for (CustomerLinksRelationship customerLinksRelationship : customerLinksRelationships) {
			// mapping data
			CustomerLinksRelationshipDTO relationshipDTO =
					mapper.map(customerLinksRelationship, CustomerLinksRelationshipDTO.class);
			// remplir member object
			if (relationshipDTO.getCategory().equals(LinkRelationshipsCategory.MEMBERS.name())
					|| relationshipDTO.getCategory()
							.equals(LinkRelationshipsCategory.GUARANTOR.name())) {
				try {
					// find customer object
					CustomerDTO member = customerService.find(relationshipDTO.getMember().getId());
					// setting data
					relationshipDTO.setMember(member);
				}
				catch (ResourcesNotFoundException e) {
					logger.error(
							CommonLoggerMessage.NOT_FOUND_OBJECT + CommonExceptionsMessage.WITH_ID,
							Customer.class.getSimpleName(), relationshipDTO.getMember().getId());
					logger.error("Customer NotFoundException : {}", e.getMessage());
				}
			}
			customerLinksRelationshipsDTOs.add(relationshipDTO);
		}
		return customerLinksRelationshipsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerLinksRelationshipService#save(com.acm.utils.dtos.
	 * CustomerLinksRelationshipDTO)
	 */
	@Override
	public CustomerLinksRelationshipDTO save(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		CustomerLinksRelationship customerLinksRelationship =
				mapper.map(customerLinksRelationshipDTO, CustomerLinksRelationship.class);
		CommonFunctions.mapperToSave(customerLinksRelationship, userClient, logger);

		CustomerLinksRelationship newCustomerLinksRelationship =
				customerLinksRelationshipRepository.save(customerLinksRelationship);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				CustomerLinksRelationship.class.getSimpleName());
		return mapper.map(newCustomerLinksRelationship, CustomerLinksRelationshipDTO.class);
	}

	/**
	 * Save all.
	 *
	 * @param customerLinksRelationshipDTOs the customer links relationship DT os
	 * @return the list
	 */
	private List<CustomerLinksRelationshipDTO> saveAll(
			List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs) {

		List<CustomerLinksRelationship> customerLinksRelationships = new ArrayList<>();
		customerLinksRelationshipDTOs.forEach(guarDTO -> customerLinksRelationships
				.add(mapper.map(guarDTO, CustomerLinksRelationship.class)));
		customerLinksRelationships
				.forEach(guar -> CommonFunctions.mapperToSave(guar, userClient, logger));

		List<CustomerLinksRelationship> newGuarantors =
				customerLinksRelationshipRepository.saveAll(customerLinksRelationships);
		List<CustomerLinksRelationshipDTO> newGuarantorsDTOs = new ArrayList<>();
		newGuarantors.forEach(newGuar -> newGuarantorsDTOs
				.add(mapper.map(newGuar, CustomerLinksRelationshipDTO.class)));
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				CustomerLinksRelationship.class.getSimpleName());
		return newGuarantorsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerLinksRelationshipService#save(java.lang.Long,
	 * com.acm.utils.dtos.CustomerLinksRelationshipDTO)
	 */
	@Override
	public CustomerLinksRelationshipDTO save(Long id,
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(customerLinksRelationshipDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update CustomerLinkRelationShip with ID = {}", id);
		CustomerLinksRelationship oldCustomerLinksRelationship =
				customerLinksRelationshipRepository.findById(id).orElse(null);

		// check if object is null
		if (oldCustomerLinksRelationship == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					CustomerLinksRelationship.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND
							+ CustomerLinksRelationship.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// mapping new data with existing data (oldCustomer)
		mapper.map(customerLinksRelationshipDTO, oldCustomerLinksRelationship);
		CommonFunctions.mapperToUpdate(oldCustomerLinksRelationship, userClient, logger);
		// update & persist data in DB
		CustomerLinksRelationship newCustomerLinksRelationship =
				customerLinksRelationshipRepository.save(oldCustomerLinksRelationship);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				CustomerLinksRelationship.class.getSimpleName());
		return mapper.map(newCustomerLinksRelationship, CustomerLinksRelationshipDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.CustomerLinksRelationshipService#deleteByIdCustomerAndCategory(com.acm.utils.
	 * dtos. CustomerLinksRelationshipDTO)
	 */
	@Override
	public void deleteByIdCustomerAndCategory(Long customerId, String category) {

		Preconditions.checkNotNull(customerId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(category, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Params customer Id : {}", customerId);
		logger.info("Params category : {}", category);
		// delete all customer link by category (Member)
		customerLinksRelationshipRepository.deleteByCustomerIdAndCategory(customerId, category);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				CustomerLinksRelationship.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.CustomerLinksRelationshipService#deleteByIdLoanAndCategory(java.lang.Long,
	 * java.lang.String)
	 */
	@Override
	public void deleteByIdLoanAndCategory(Long idLoan, String category) {

		Preconditions.checkNotNull(idLoan, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(category, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Params Loan Id : {}", idLoan);
		logger.info("Params category : {}", category);
		customerLinksRelationshipRepository.deleteByIdLoanAndCategory(idLoan, category);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				CustomerLinksRelationship.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.CustomerLinksRelationshipService#findAllMembersByCustomerMembersId(com.acm.
	 * utils.dtos.CustomerLinksRelationshipDTO)
	 */
	@Override
	public List<CustomerLinksRelationshipDTO> findAllMembersByCustomerMembersId(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		Preconditions.checkNotNull(customerLinksRelationshipDTO.getMember().getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		List<CustomerLinksRelationship> customerLinksRelationships =
				customerLinksRelationshipRepository.findAllMembersByCustomerMembersId(
						customerLinksRelationshipDTO.getMember().getId(),
						customerLinksRelationshipDTO.getCategory());
		List<CustomerLinksRelationshipDTO> customerLinksRelationshipsDTOs = new ArrayList<>();
		customerLinksRelationships
				.forEach(customerLinksRelationship -> customerLinksRelationshipsDTOs.add(
						mapper.map(customerLinksRelationship, CustomerLinksRelationshipDTO.class)));
		return customerLinksRelationshipsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.CustomerLinksRelationshipService#findAllActiveGuarantors(com.acm.utils.dtos.
	 * CustomerLinksRelationshipDTO)
	 */
	@Override
	public List<CustomerLinksRelationshipDTO> findAllActiveGuarantors(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		Preconditions.checkNotNull(customerLinksRelationshipDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QCustomerLinksRelationship
		QCustomerLinksRelationship qCustomerLinksRelationship =
				QCustomerLinksRelationship.customerLinksRelationship;
		// init idAccounts list
		List<Long> idAccounts = new ArrayList<>();
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		List<Integer> workflowStatus = new ArrayList<>();
		workflowStatus
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey());
		workflowStatus
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey());
		workflowStatus
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey());
		// find only enabled data
		predicate.and(qCustomerLinksRelationship.enabled.eq(Boolean.TRUE));
		// find only date fin is null (Active GUARANTORS)
		predicate.and(qCustomerLinksRelationship.dateFin.isNull());
		// find by id member
		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getMember())
				&& !ACMValidationUtils
						.isNullOrEmpty(customerLinksRelationshipDTO.getMember().getId())) {
			predicate.and(qCustomerLinksRelationship.member
					.eq(new Customer(customerLinksRelationshipDTO.getMember().getId())));
		}
		else {
			return new ArrayList<>();
		}
		// find by id Category
		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getCategory())) {
			predicate.and(qCustomerLinksRelationship.category
					.eq(customerLinksRelationshipDTO.getCategory()));
		}
		else {
			return new ArrayList<>();
		}
		// returning data
		Iterable<CustomerLinksRelationship> iterable =
				customerLinksRelationshipRepository.findAll(predicate);
		List<CustomerLinksRelationship> customerLinksRelationships = new ArrayList<>();
		iterable.forEach(customerLinksRelationships::add);
		logger.info("findAllActiveGuarantors : {} : CustomerLinksRelationship was founded",
				customerLinksRelationships.size());
		// mapping data
		List<CustomerLinksRelationshipDTO> customerLinksRelationshipsDTOs = new ArrayList<>();
		customerLinksRelationships
				.forEach(customerLinksRelationship -> customerLinksRelationshipsDTOs.add(
						mapper.map(customerLinksRelationship, CustomerLinksRelationshipDTO.class)));
		// check if GUARANTOR is active in ABACUS
		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipsDTOs)) {
			Iterator<CustomerLinksRelationshipDTO> it = customerLinksRelationshipsDTOs.iterator();
			while (it.hasNext()) {
				CustomerLinksRelationshipDTO link = it.next();
				// find loan ID_Account

				LoanDTO loanDTO = new LoanDTO();
				try {
					loanDTO = loanService.find(link.getIdLoan());
					// check if loan is cancelled / rejected / declined in ACM
					if (!workflowStatus.contains(loanDTO.getStatutWorkflow())) {
						// if loan is not cancelled / rejected / declined in ACM
						idAccounts.add(loanDTO.getIdAccountExtern());
					}
					else {
						it.remove();
					}
				}
				catch (ResourcesNotFoundException e) {
					logger.error(
							CommonLoggerMessage.NOT_FOUND_OBJECT + CommonExceptionsMessage.WITH_ID,
							Loan.class.getSimpleName(), link.getIdLoan());
					logger.error("Loan NotFoundException : {}", e.getMessage());
				}
			}
			if (!ACMValidationUtils.isNullOrEmpty(idAccounts)) {
				// check & return data
				if (transversClient.findActiveAccountForCustomerAndLoan(idAccounts) == 0) {
					// returning empty list
					return new ArrayList<>();
				}
			}

		}
		return customerLinksRelationshipsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.CustomerLinksRelationshipService#findAllLoanGuarantors(com.acm.utils.dtos.
	 * CustomerLinksRelationshipDTO)
	 */
	@Override
	public List<CustomerLinksRelationshipDTO> findAllLoanGuarantors(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		Preconditions.checkNotNull(customerLinksRelationshipDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// Check Customer Id
		if (ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getCustomerId())) {
			return new ArrayList<>();
		}
		// init QCustomerLinksRelationship
		QCustomerLinksRelationship qCustomerLinksRelationship =
				QCustomerLinksRelationship.customerLinksRelationship;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		List<LoanDTO> loanDTOs =
				loanService.findByIdCustomer(customerLinksRelationshipDTO.getCustomerId());

		List<Long> loanIds = new ArrayList<>();
		if (ACMValidationUtils.isNullOrEmpty(loanDTOs)) {
			return new ArrayList<>();
		}
		for (LoanDTO loanDTO : loanDTOs) {
			loanIds.add(loanDTO.getLoanId());
		}
		// find by id Loan
		predicate.and(qCustomerLinksRelationship.idLoan.in(loanIds));
		// find by id Category
		predicate.and(qCustomerLinksRelationship.category.eq(CommonConstants.RELATION_GUARANTOR));

		Iterable<CustomerLinksRelationship> iterable =
				customerLinksRelationshipRepository.findAll(predicate);
		List<CustomerLinksRelationship> customerLinksRelationships = new ArrayList<>();
		iterable.forEach(customerLinksRelationships::add);
		logger.info("{} : All loan guarantors was founded", customerLinksRelationships.size());

		List<CustomerLinksRelationshipDTO> customerLinksRelationshipsDTOs = new ArrayList<>();
		for (CustomerLinksRelationship customerLinksRelationship : customerLinksRelationships) {
			// mapping data
			CustomerLinksRelationshipDTO relationshipDTO =
					mapper.map(customerLinksRelationship, CustomerLinksRelationshipDTO.class);
			// remplir member object
			if (relationshipDTO.getCategory().equals(LinkRelationshipsCategory.MEMBERS.name())
					|| relationshipDTO.getCategory()
							.equals(LinkRelationshipsCategory.GUARANTOR.name())) {
				try {
					// find customer object
					CustomerDTO member =
							customerService.findCustomer(relationshipDTO.getMember().getId());
					// setting data
					relationshipDTO.setMember(member);
				}
				catch (ResourcesNotFoundException e) {
					logger.error(
							CommonLoggerMessage.NOT_FOUND_OBJECT + CommonExceptionsMessage.WITH_ID,
							Customer.class.getSimpleName(), relationshipDTO.getMember().getId());
					logger.error("ResourcesNotFoundException : {}", e.getMessage());
				}
			}
			// remplir loan object
			relationshipDTO.setLoanDTO(loanDTOs.stream()
					.filter(loanDTO -> loanDTO.getLoanId().equals(relationshipDTO.getIdLoan()))
					.findAny().orElse(null));
			customerLinksRelationshipsDTOs.add(relationshipDTO);
		}
		return customerLinksRelationshipsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerLinksRelationshipService#findGuarantees(com.acm.utils.dtos.
	 * CustomerLinksRelationshipDTO)
	 */
	@Override
	public List<CustomerLinksRelationshipDTO> findGuarantees(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(customerLinksRelationshipDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// Check member
		if (ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getMember())
				&& ACMValidationUtils
						.isNullOrEmpty(customerLinksRelationshipDTO.getMember().getId())) {
			return new ArrayList<>();
		}
		// init QCustomerLinksRelationship
		QCustomerLinksRelationship qCustomerLinksRelationship =
				QCustomerLinksRelationship.customerLinksRelationship;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qCustomerLinksRelationship.enabled.eq(Boolean.TRUE));
		// find by id member
		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getMember())
				&& !ACMValidationUtils
						.isNullOrEmpty(customerLinksRelationshipDTO.getMember().getId())) {
			predicate.and(qCustomerLinksRelationship.member
					.eq(new Customer(customerLinksRelationshipDTO.getMember().getId())));
		}

		// find by id Category
		predicate.and(
				qCustomerLinksRelationship.category.eq(LinkRelationshipsCategory.GUARANTOR.name()));

		Iterable<CustomerLinksRelationship> iterable =
				customerLinksRelationshipRepository.findAll(predicate);
		List<CustomerLinksRelationship> customerLinksRelationships = new ArrayList<>();
		iterable.forEach(customerLinksRelationships::add);
		logger.info("{} : CustomerLinksRelationship was founded",
				customerLinksRelationships.size());
		List<CustomerLinksRelationshipDTO> customerLinksRelationshipsDTOs = new ArrayList<>();
		for (CustomerLinksRelationship customerLinksRelationship : customerLinksRelationships) {
			// mapping data
			CustomerLinksRelationshipDTO relationshipDTO =
					mapper.map(customerLinksRelationship, CustomerLinksRelationshipDTO.class);
			// remplir member object
			if (relationshipDTO.getCategory().equals(LinkRelationshipsCategory.MEMBERS.name())
					|| relationshipDTO.getCategory()
							.equals(LinkRelationshipsCategory.GUARANTOR.name())) {
				try {
					// find customer object
					CustomerDTO member =
							customerService.findCustomer(relationshipDTO.getMember().getId());
					// setting data
					relationshipDTO.setMember(member);
				}
				catch (ResourcesNotFoundException e) {
					logger.error(
							CommonLoggerMessage.NOT_FOUND_OBJECT + CommonExceptionsMessage.WITH_ID,
							Customer.class.getSimpleName(), relationshipDTO.getMember().getId());
					logger.error("ResourcesNotFoundException : {}", e.getMessage());
				}
			}
			// set loan
			relationshipDTO.setLoanDTO(loanService.find(relationshipDTO.getIdLoan()));
			customerLinksRelationshipsDTOs.add(relationshipDTO);
		}
		return customerLinksRelationshipsDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerLinksRelationshipService#cancelGuarantorLoan(com.acm.utils.dtos.
	 * CustomerLinksRelationshipDTO)
	 */
	@Override
	public void cancelGuarantorLoan(CustomerLinksRelationshipDTO customerLinksRelationshipDTO)
			throws ResourcesNotFoundException {

		if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getIdLoan())
				&& !ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTO.getMember())
				&& !ACMValidationUtils
						.isNullOrEmpty(customerLinksRelationshipDTO.getMember().getId())) {
			// Find GUARANTOR LINK
			List<CustomerLinksRelationship> customerLinksRelationships =
					customerLinksRelationshipRepository.findByIdLoanAndCategoryAndMemberAndEnabled(
							customerLinksRelationshipDTO.getIdLoan(),
							LinkRelationshipsCategory.GUARANTOR.name(),
							new Customer(customerLinksRelationshipDTO.getMember().getId()),
							Boolean.TRUE);
			// Disable GUARANTOR / LOAN LINK
			if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationships)) {
				for (CustomerLinksRelationship oldCustomerLinksRelationship : customerLinksRelationships) {
					logger.info("Update CustomerLinkRelationShip with ID = {}",
							oldCustomerLinksRelationship.getId());
					CommonFunctions.mapperToUpdate(oldCustomerLinksRelationship, userClient,
							logger);
					// update & persist data in DB
					oldCustomerLinksRelationship.setEnabled(Boolean.FALSE);
					customerLinksRelationshipRepository.save(oldCustomerLinksRelationship);
					logger.info(
							"Links Relationship between Loan with ID = {} and Gurantor with ID={} was DISABLED",
							customerLinksRelationshipDTO.getIdLoan(),
							customerLinksRelationshipDTO.getMember().getId());
				}
			}

			// find loan data
			LoanDTO loanDTO = loanService.find(customerLinksRelationshipDTO.getIdLoan());
			// assign to default ACCOUNTPORTFOLIO_CODE
			UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
			loanDTO.setOwner(foundedUserDTO.getLogin());
			loanDTO.setOwnerName(foundedUserDTO.getSimpleName());
			loanService.save(loanDTO.getLoanId(), loanDTO);

		}
	}

	/**
	 * Gets the user by portfolio id otherwise returning default user.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the username by portfolio id
	 */
	private UserDTO getUsernameByPortfolioId(LoanDTO loanDTO) {

		// default ADMIN user
		String userName = CommonConstants.DEFAULT_USER;
		// find user by AccountPortfolioId
		if (loanDTO.getPortfolioId() != null && loanDTO.getPortfolioId() != 0) {
			UserDTO params = new UserDTO();
			params.setAccountPortfolioId(loanDTO.getPortfolioId());
			List<UserDTO> userDTOs = userClient.find(params);
			// check && setting
			if (!ACMValidationUtils.isNullOrEmpty(userDTOs)) {
				// setting userName
				return userDTOs.get(0);
			}
		}
		logger.info(" founded USER_NAME = [{}]", userName);
		return userClient.findByLogin(userName);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.CustomerLinksRelationshipService#findGuarantorsFromIBAndSaveInAcm(com.acm.
	 * utils.dtos.CustomerLinksRelationshipDTO)
	 */
	@Override
	public List<CustomerLinksRelationshipDTO> findGuarantorsFromIBAndSaveInAcm(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		// find guarantors from IB
		List<CustomerLinksRelationshipDTO> ibGuarantors =
				loadDataIBService.findGuarantors(customerLinksRelationshipDTO);
		// delete guarantors in ACM
		deleteByIdLoanAndCategory(customerLinksRelationshipDTO.getIdLoan(),
				CommonConstants.RELATION_GUARANTOR);
		// save guarantors got from IB in ACM
		return saveAll(ibGuarantors);
	}

}
