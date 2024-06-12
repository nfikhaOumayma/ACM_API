/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.acm.client.CreditClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.ExpenseDrAndCrAccountsEmptyException;
import com.acm.exceptions.type.ExpensesLimitNotFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ExpensesRepository;
import com.acm.service.ExpensesLimitsService;
import com.acm.service.ExpensesService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.ExpensesCountDTO;
import com.acm.utils.dtos.ExpensesDTO;
import com.acm.utils.dtos.ExpensesLimitDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.JournalEnteriesInformationDTO;
import com.acm.utils.dtos.JournalEnteriesToAbacusDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.ExpensesPaginationDTO;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.models.Expenses;
import com.acm.utils.models.ExpensesLimit;
import com.acm.utils.models.Loan;
import com.acm.utils.models.QExpenses;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link ExpensesServiceImpl} class.
 * 
 * @author Ines Dridi
 * @since 1.1.3
 */
@Service
public class ExpensesServiceImpl implements ExpensesService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ExpensesServiceImpl.class);

	/** The expenses repository. */
	@Autowired
	private ExpensesRepository expensesRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The expenses limits service. */
	@Autowired
	private ExpensesLimitsService expensesLimitsService;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ExpensesService#find(com.acm.utils.dtos.pagination.ExpensesPaginationDTO)
	 */
	@Override
	public ExpensesPaginationDTO find(ExpensesPaginationDTO expensesPaginationDTO) {

		Preconditions.checkNotNull(expensesPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(expensesPaginationDTO.getPageNumber())) {
			expensesPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(expensesPaginationDTO.getPageSize())) {
			expensesPaginationDTO.setPageSize(10);
		}
		// setting default data
		expensesPaginationDTO.setResultsExpenses(new ArrayList<>());
		// setting default totals pages
		expensesPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		expensesPaginationDTO.setTotalPages(0);
		// if statut is empty
		if (ACMValidationUtils.isNullOrEmpty(expensesPaginationDTO.getParams().getStatut())) {
			return expensesPaginationDTO;
		}
		QExpenses qExpenses = QExpenses.expenses;
		BooleanBuilder predicate = buildQuery(expensesPaginationDTO.getParams(), qExpenses);
		// check if predicate is null
		if (ACMValidationUtils.isNullOrEmpty(predicate)) {
			return expensesPaginationDTO;
		}
		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(expensesPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(expensesPaginationDTO.getSortField())) {
			pageable = PageRequest.of(expensesPaginationDTO.getPageNumber(),
					expensesPaginationDTO.getPageSize(), Sort.Direction.ASC,
					expensesPaginationDTO.getSortField());
		}
		else if ("-1".equals(expensesPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(expensesPaginationDTO.getSortField())) {
			pageable = PageRequest.of(expensesPaginationDTO.getPageNumber(),
					expensesPaginationDTO.getPageSize(), Sort.Direction.DESC,
					expensesPaginationDTO.getSortField());
		}
		else {
			// default sort by dateInsertion : DESC
			pageable = PageRequest.of(expensesPaginationDTO.getPageNumber(),
					expensesPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateInsertion");
		}
		// load data
		Page<Expenses> pagedResult = expensesRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Expenses> expenses = pagedResult.getContent();
			logger.info("{} : Expenses was founded (PageNumber = {} / PageSize = {} )",
					expenses.size(), expensesPaginationDTO.getPageNumber(),
					expensesPaginationDTO.getPageSize());
			List<ExpensesDTO> expensesDTOs = new ArrayList<>();
			expenses.forEach(expense -> expensesDTOs.add(mapper.map(expense, ExpensesDTO.class)));
			// setting data
			expensesPaginationDTO.setResultsExpenses(expensesDTOs);
			// setting totals pages
			expensesPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			expensesPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return expensesPaginationDTO;
	}

	/**
	 * Builds the query.
	 * 
	 * @author Ines Dridi
	 * @param expensesDTO the expenses DTO
	 * @param qExpenses the q expenses
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(ExpensesDTO expensesDTO, QExpenses qExpenses) {

		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// get groupe of user
		GroupeDTO groupeDTO = userDTO.getGroupes().iterator().next();
		// get expenses by TELLER if connected user is a TELLER
		if (groupeDTO.getCode().equals("TELLER")) {
			// add connected user
			predicate.and(qExpenses.teller.eq(userDTO.getLogin()));
		}
		// if connected user FINANCIAL (OR Any other group having access to expense list)
		else /* if (groupeDTO.getCode().equals("FINANCIAL")) */ {
			// find loan by Access Branches for connected user
			if (!ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
				int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(","))
						.stream().map(String::trim).mapToInt(Integer::parseInt).toArray();
				logger.info("ID Access Branches = {}", arrayBranchIds);
				List<Long> listBranchIds = new ArrayList<>(arrayBranchIds.length);
				for (int i : arrayBranchIds) {
					listBranchIds.add(Long.valueOf(i));
				}
				// setting predicate to find by given branch Id
				predicate.and(qExpenses.idBranch.in(listBranchIds));
			}
		}
		// of connected user not TELLER or FINANCIAL
		// else {
		// return null;
		// }
		// expensesParams.description
		if (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getDescription())) {
			predicate.and(qExpenses.description.like("%" + expensesDTO.getDescription() + "%"));
		}
		// expensesParams.expensesAmount
		if (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getExpensesAmount())) {
			predicate.and(qExpenses.expensesAmount.eq(expensesDTO.getExpensesAmount()));
		}
		// expensesParams.statut
		predicate.and(qExpenses.statut.eq(expensesDTO.getStatut()));

		// find and by expensesType
		if (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getIdBranch())) {
			predicate.and(qExpenses.idBranch.eq(expensesDTO.getIdBranch()));
		}
		// expensesParams.teller
		if (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getTellerName())) {
			predicate.and(qExpenses.tellerName.like("%" + expensesDTO.getTellerName() + "%"));
		}
		// expensesParams.expensesTypeLibelle
		if (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getExpensesTypeLibelle())) {
			predicate.and(qExpenses.expensesTypeLibelle
					.like("%" + expensesDTO.getExpensesTypeLibelle() + "%"));
		}
		// expensesParams.applyDate
		if (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getApplyDate())) {
			java.sql.Date sqlDate = java.sql.Date
					.valueOf(DateUtil.convertToLocalDateViaInstant(expensesDTO.getApplyDate()));
			predicate.and(qExpenses.applyDate.eq(sqlDate));
		}
		// find and by expensesType
		if (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getIdExpensesType())) {
			predicate.and(qExpenses.idExpensesType.eq(expensesDTO.getIdExpensesType()));
		}
		// find expenses by owner
		if (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getOwnerName())) {
			predicate.and(qExpenses.ownerName.like("%" + expensesDTO.getOwnerName() + "%"));
		}
		// expensesParams.id
		if (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getId())) {
			predicate.and(qExpenses.id.like("%" + expensesDTO.getId() + "%"));
		}
		return predicate;
	}

	/**
	 * Date formatter.
	 *
	 * @return the string
	 */
	String dateFormatter() {

		String pattern = "dd/MM/yyyy";
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);
		String date = simpleDateFormat.format(new Date());
		return date;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesService#save(java.lang.Long, com.acm.utils.dtos.ExpensesDTO)
	 */
	@Override
	public ExpensesDTO save(Long id, ExpensesDTO expensesDTO)
			throws ResourcesNotFoundException, ApiAbacusException,
			ExpenseDrAndCrAccountsEmptyException, ExpensesLimitNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(expensesDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update expenses  with ID = {}", id);
		Expenses oldExpenses = expensesRepository.findById(id).orElse(null);
		// check if object is null
		if (oldExpenses == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Expenses.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Expenses.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldExpenses)
		// if expenses is rejected => reset restLimit of expensesType
		if (expensesDTO.getStatut() == -1 && oldExpenses.getStatut() != -1) {
			ExpensesLimitDTO params = new ExpensesLimitDTO();
			params.setIdBranch(expensesDTO.getIdBranch());
			params.setIdExpensesType(expensesDTO.getIdExpensesType());
			List<ExpensesLimitDTO> expensesLimitDTOs = expensesLimitsService.find(params);
			if (!ACMValidationUtils.isNullOrEmpty(expensesLimitDTOs)) {
				// get first result
				ExpensesLimitDTO newExpensesLimitDTO = expensesLimitDTOs.get(0);
				// setting limite
				newExpensesLimitDTO.setRestLimit(
						newExpensesLimitDTO.getRestLimit() + expensesDTO.getExpensesAmount());
				// save new expenses Limits
				expensesLimitsService.save(newExpensesLimitDTO);
			}
		}
		// Default value
		String action = "";
		if ((ACMValidationUtils.isNullOrEmpty(oldExpenses.getOwner()))
				&& (!ACMValidationUtils.isNullOrEmpty(expensesDTO.getOwner()))) {
			action = "ASSIGN";
		}
		else if (expensesDTO.getStatut() == -1 && oldExpenses.getStatut() != -1) {
			action = "REJECT";
		}
		else if (expensesDTO.getStatut() == 1 && oldExpenses.getStatut() != 1) {
			action = "ACCPET";
			// Create And Post from Abacus
			// find credit account and debit account by expenses type id
			ExpensesLimitDTO expensesLimitDTOParams = new ExpensesLimitDTO();
			expensesLimitDTOParams.setIdBranch(expensesDTO.getIdBranch());
			expensesLimitDTOParams.setIdExpensesType(expensesDTO.getIdExpensesType());
			// find limit expensesLimitDto by branch id and expenses type id
			ExpensesLimitDTO expensesLimitDto =
					expensesLimitsService.findByTypeAndBranchId(expensesLimitDTOParams);
			// check if CR Or DR is null then throw exception
			if (ACMValidationUtils.isNullOrEmpty(expensesLimitDto.getCr())
					|| ACMValidationUtils.isNullOrEmpty(expensesLimitDto.getDr())) {
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
						ExpensesLimit.class.getSimpleName());
				throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						CommonExceptionsMessage.NOT_FOUND + ExpensesLimit.class.getSimpleName());
			}
			// object expenses journal page
			JournalEnteriesToAbacusDTO expensesJournalPageDTO = new JournalEnteriesToAbacusDTO();
			expensesJournalPageDTO.setJournalEntry(new ArrayList<JournalEnteriesInformationDTO>());
			JournalEnteriesInformationDTO journaEnterieInformation =
					new JournalEnteriesInformationDTO();
			journaEnterieInformation.setReference(expensesDTO.getNote());
			Long accountId = transversClient.findAccountId(expensesDTO.getIdBranch().intValue(),
					expensesLimitDto.getCr());
			journaEnterieInformation.setAccountID(accountId);
			journaEnterieInformation.setCredit(true);
			journaEnterieInformation.setValueDate(dateFormatter());
			journaEnterieInformation
					.setCurrencyAmount(new BigDecimal(expensesDTO.getExpensesAmount()));
			journaEnterieInformation.setCurrencyID(1);
			journaEnterieInformation.setExchangeRate(1);
			journaEnterieInformation.setBranchID(expensesDTO.getIdBranch());
			journaEnterieInformation.setJournalID(1);
			journaEnterieInformation.setIsIBTEntryRequired(false);
			journaEnterieInformation.setIsIBTEntry(false);
			journaEnterieInformation.setReference(expensesDTO.getExpensesTypeLibelle());
			journaEnterieInformation.setDescription(expensesDTO.getExpensesTypeLibelle());
			expensesJournalPageDTO.setJournalID(3L);
			expensesJournalPageDTO.setPageDescription(expensesDTO.getExpensesTypeLibelle());
			journaEnterieInformation.setAmount(new BigDecimal(expensesDTO.getExpensesAmount()));
			expensesJournalPageDTO.getJournalEntry().add(journaEnterieInformation);

			JournalEnteriesInformationDTO journaEnterieInformationDebit =
					new JournalEnteriesInformationDTO();

			mapper.map(journaEnterieInformation, journaEnterieInformationDebit);
			journaEnterieInformationDebit.setCredit(false);
			journaEnterieInformationDebit.setAccountID(transversClient
					.findAccountId(expensesDTO.getIdBranch().intValue(), expensesLimitDto.getDr()));
			expensesJournalPageDTO.getJournalEntry().add(journaEnterieInformationDebit);
			// calling api abacus post journalpage
			try {
				transversClient.createJournalEntry(expensesJournalPageDTO);
			}
			catch (Exception e) {
				logger.error("Error will calling API ABACUS for update LOAN: {}", e.getMessage());
				e.printStackTrace();
				throw new ApiAbacusException(
						new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
								CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
								new TechnicalException()),
						CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
			}

		}
		mapper.map(expensesDTO, oldExpenses);
		CommonFunctions.mapperToUpdate(oldExpenses, userClient, logger);
		Expenses newExpenses = expensesRepository.save(oldExpenses);

		// Send notification
		if (!"".equals(action)) {
			sendNotification(action, expensesDTO);
		}
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
		return mapper.map(newExpenses, ExpensesDTO.class);
	}

	/**
	 * Send notification.
	 * 
	 * @author Ines Dridi
	 * @param action the action
	 * @param expensesDTO the expenses DTO
	 */
	private void sendNotification(String action, ExpensesDTO expensesDTO) {

		String actionDescription = "";
		switch (action) {
			case "ASSIGN":
				// Notification assign expenses
				actionDescription = "Expenses has been assigned at "
						+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
						+ expensesDTO.getOwner();
				NotificationsDTO notificationsDTOAssigned =
				creditClient.create(new NotificationsDTO(expensesDTO.getTeller(),
						NotificationCategory.EXPENSES.name(), NotificationType.INFO.name(),
						Boolean.TRUE,
						CommonConstants.ACM_NOTIFICATION_ACTION_ASSIGN_EXPENSES,
						actionDescription, expensesDTO.getId()));
				logger.info("New assigned Notification  [{}] has been inserted.",
						notificationsDTOAssigned);
				break;

			case "REJECT":
				// Notification reject Expenses
				actionDescription = "Expenses has been rejected at "
						+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
						+ expensesDTO.getOwner();
				NotificationsDTO notificationsDTORejected =
						creditClient.create(new NotificationsDTO(expensesDTO.getTeller(),
								NotificationCategory.EXPENSES.name(), NotificationType.INFO.name(),
								Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_ACTION_REJECT_EXPENSES,
								actionDescription, expensesDTO.getId()));
				logger.info("New REJECT Notification  [{}] has been inserted.",
						notificationsDTORejected);
				break;

			case "ACCEPT":
				// Notification accept Expenses
				actionDescription = "Expenses has been accepted at "
						+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
						+ expensesDTO.getOwner();
				NotificationsDTO notificationsDTOAccepted =
						creditClient.create(new NotificationsDTO(expensesDTO.getTeller(),
								NotificationCategory.EXPENSES.name(), NotificationType.INFO.name(),
								Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_ACTION_ACCEPT_EXPENSES,
								actionDescription, expensesDTO.getId()));
				logger.info("New ACCEPT Notification  [{}] has been inserted.",
						notificationsDTOAccepted);
				break;

			default:
				break;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesService#save(com.acm.utils.dtos.ExpensesDTO)
	 */
	@Override
	public ExpensesDTO save(ExpensesDTO expensesDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(expensesDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Create new expenses ");
		// save Expenses
		Expenses expenses = mapper.map(expensesDTO, Expenses.class);
		CommonFunctions.mapperToSave(expenses, userClient, logger);
		Expenses newExpenses = expensesRepository.save(expenses);

		// update limit expenses per branch and type of expenses
		ExpensesLimitDTO expensesLimit = new ExpensesLimitDTO();
		expensesLimit.setIdBranch(expensesDTO.getIdBranch());
		expensesLimit.setIdExpensesType(expensesDTO.getIdExpensesType());
		List<ExpensesLimitDTO> expensesLimitDTOs = expensesLimitsService.find(expensesLimit);
		if (!ACMValidationUtils.isNullOrEmpty(expensesLimitDTOs)) {
			// get first result
			ExpensesLimitDTO updateExpensesLimitDTO = expensesLimitDTOs.get(0);
			// setting limite
			updateExpensesLimitDTO.setRestLimit(
					updateExpensesLimitDTO.getRestLimit() - expensesDTO.getExpensesAmount());
			// save new expenses Limits
			expensesLimitsService.save(updateExpensesLimitDTO.getId(), updateExpensesLimitDTO);
		}
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Loan.class.getSimpleName());
		return mapper.map(newExpenses, ExpensesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesService#count()
	 */
	@Override
	public ExpensesCountDTO count() {

		// init QExpenses
		QExpenses qExpenses = QExpenses.expenses;
		ExpensesCountDTO expensesCountDTO = new ExpensesCountDTO(0L, 0L, 0L);
		try {

			// statut 1 : New
			expensesCountDTO.setCountNew(
					expensesRepository.count(buildQuery(new ExpensesDTO(0), qExpenses)));

			// statut 2 : Accepted
			expensesCountDTO.setCountAccepted(
					expensesRepository.count(buildQuery(new ExpensesDTO(1), qExpenses)));

			// statut 3 : Rejected
			expensesCountDTO.setCountRejected(
					expensesRepository.count(buildQuery(new ExpensesDTO(-1), qExpenses)));
		}
		catch (Exception e) {
			logger.info("ERROR Returning count expenses by status {}", expensesCountDTO);
			return expensesCountDTO;
		}
		logger.info("Returning count expenses by status {}", expensesCountDTO);

		return expensesCountDTO;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExpensesService#findAccountGlList(java.lang.Long)
	 */
	@Override
	public List<String> findAccountGlList(Long branchId) {

		logger.info("start find Account Gl List ");
		try {
			List<String> accountGl = transversClient.findAccountGlList(branchId);
			logger.info("number of Gl Account {}", accountGl.size());
			return accountGl;
		}
		catch (Exception e) {
			logger.error("### Find Account Gl List ERROR : {}", e.getMessage());
			return new ArrayList<>();
		}
	}

	@Override
	public ExpensesDTO findExpensesById(Long id) {
		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Expense By Id : {}", id);
		Expenses expenses = expensesRepository.findById(id).get();
		ExpensesDTO expensesDTO = new ExpensesDTO();
		expensesDTO = mapper.map(expenses, ExpensesDTO.class);
		logger.info("METHOD : findExpensesById : {} : Expense was founded", expensesDTO);
		return expensesDTO;
	}
}
