/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonFunctions;
import com.acm.repository.LoanRepository;
import com.acm.service.AnalyticsService;
import com.acm.service.CustomerService;
import com.acm.service.IBLoanService;
import com.acm.service.LoanParticipantsService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.LoanAnalyticsDTO;
import com.acm.utils.dtos.LoanParticipantsDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;
import com.acm.utils.models.Loan;
import com.acm.utils.models.QLoan;
import com.acm.utils.models.QLoanParticipants;
import com.acm.utils.validation.ACMValidationUtils;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.jpa.JPAExpressions;
import com.querydsl.jpa.impl.JPAQueryFactory;

/**
 * {@link AnalyticsServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.1.3
 */
@Service
public class AnalyticsServiceImpl implements AnalyticsService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AnalyticsServiceImpl.class);

	/** The loan repository. */
	@Autowired
	private LoanRepository loanRepository;

	/** The ib loan service. */
	@Autowired
	private IBLoanService ibLoanService;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The loan participants service. */
	@Autowired
	private LoanParticipantsService loanParticipantsService;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#totalAppliedLoans()
	 */
	@Override
	public LoanAnalyticsDTO totalAppliedLoans() {

		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// init List BranchIds
		List<Integer> branchIds = initListBranchIds(connectedUser);
		// init owners
		List<UserDTO> userDTOs = userClient.findUsers();
		List<String> owners = initListOwners(userDTOs);
		Float total = (float) (loanRepository.countByEnabledAndBranchIDInOrOwnerIn(Boolean.TRUE,
				branchIds, owners));
		Float ibLoanRejectedcount = total + ibLoanService.countByStatutAndBranchIDsAndOwners(
				CommonConstants.STATUT_IB_REJECT, branchIds, owners);

		return new LoanAnalyticsDTO(total,
				CommonFunctions.calculateAndFormatPourcentage(total, ibLoanRejectedcount, 1));
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#totalApprovedLoans()
	 */
	@Override
	public LoanAnalyticsDTO totalApprovedLoans() {

		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// init List BranchIds
		List<Integer> branchIds = initListBranchIds(connectedUser);
		// init owners
		List<UserDTO> userDTOs = userClient.findUsers();
		List<String> owners = initListOwners(userDTOs);
		// init list status
		List<Integer> listStatut = new ArrayList<>();
		// loan approved => StatutWorkflow in CUSTOMER_DECISION or UPLOAD_SIGNED_AGREEMENT or
		// DISBURSEMENT_CASE_CLOSURE
		listStatut.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION)
				.getKey());
		listStatut.add(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT).getKey());
		listStatut.add(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE).getKey());
		listStatut.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey());

		// all loan
		Float total = (float) (loanRepository.countByEnabledAndBranchIDInOrOwnerIn(Boolean.TRUE,
				branchIds, owners));
		Float totalApproved = (float) loanRepository
				.countByStatutWorkflowInAndEnabledAndBranchIDInOrOwnerInAndStatutWorkflowIn(
						listStatut, Boolean.TRUE, branchIds, owners, listStatut);

		return new LoanAnalyticsDTO(totalApproved,
				CommonFunctions.calculateAndFormatPourcentage(totalApproved, total, 1));
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#totalCanceledRejectedLoans()
	 */
	@Override
	public LoanAnalyticsDTO totalCanceledRejectedLoans() {

		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// init List BranchIds
		List<Integer> branchIds = initListBranchIds(connectedUser);
		// init owners
		List<UserDTO> userDTOs = userClient.findUsers();
		List<String> owners = initListOwners(userDTOs);
		// init list status
		List<Integer> listStatut = new ArrayList<>();
		// loan approved => StatutWorkflow in REJECTED or CANCELLED or DECLINE
		listStatut.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey());
		listStatut
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey());
		listStatut.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey());

		// all loan
		Float total = (float) (loanRepository.countByEnabledAndBranchIDInOrOwnerIn(Boolean.TRUE,
				branchIds, owners));
		Float totalCanceledRejected = (float) loanRepository
				.countByStatutWorkflowInAndEnabledAndBranchIDInOrOwnerInAndStatutWorkflowIn(
						listStatut, Boolean.TRUE, branchIds, owners, listStatut);

		return new LoanAnalyticsDTO(totalCanceledRejected,
				CommonFunctions.calculateAndFormatPourcentage(totalCanceledRejected, total, 1));
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#loansByProducts()
	 */
	@Override
	public LoanAnalyticsDTO loansByProducts() {

		// find all validate product
		List<ProductDTO> productDTOs = parametrageClient.findProducts(new ProductDTO());
		// mapping & processing data
		List<String> labelsProduct = new ArrayList<>();
		List<Integer> loansByProduct = new ArrayList<>();
		for (ProductDTO productDTO : productDTOs) {
			// label product
			labelsProduct.add(productDTO.getDescription());
			// find loans by product
			Long count = loanRepository.countByProductIdAndEnabled(productDTO.getId().intValue(),
					Boolean.TRUE);
			loansByProduct.add(count.intValue());
		}
		return new LoanAnalyticsDTO(loansByProduct, labelsProduct);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#loansStatByMonths()
	 */
	@Override
	public LoanAnalyticsDTO loansStatByMonths() {

		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// init List BranchIds
		List<Integer> branchIds = initListBranchIds(connectedUser);
		// init owners
		List<UserDTO> userDTOs = userClient.findUsers();
		List<String> owners = initListOwners(userDTOs);

		// init list status REJECTED / CANCELLED / DECLINE
		List<Integer> listStatutRejectedCanceled = new ArrayList<>();
		// loan canceled => StatutWorkflow in REJECTED or CANCELLED or DECLINE
		listStatutRejectedCanceled
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey());
		listStatutRejectedCanceled
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey());
		listStatutRejectedCanceled
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey());

		// init list status
		List<Integer> listStatutApproved = new ArrayList<>();
		// loan approved => StatutWorkflow in CUSTOMER_DECISION or UPLOAD_SIGNED_AGREEMENT or
		// DISBURSEMENT_CASE_CLOSURE
		listStatutApproved.add(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION).getKey());
		listStatutApproved.add(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT).getKey());
		listStatutApproved.add(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE).getKey());
		listStatutApproved
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey());
		// init list
		List<String> xaxisCategories = new ArrayList<>();
		List<Integer> seriesAppliedLoans = new ArrayList<>();
		List<Integer> seriesApprovedLoans = new ArrayList<>();
		List<Integer> seriesCanceledRejectedLoans = new ArrayList<>();
		// init QLoan
		QLoan qLoan = QLoan.loan;

		YearMonth thisMonth = YearMonth.now();
		DateTimeFormatter monthYearFormatter = DateTimeFormatter.ofPattern("MMM yyyy");
		// init last 11 months DATA
		for (int i = 11; i > 0; i--) {
			YearMonth lastMonth = thisMonth.minusMonths(i);
			// setting month label
			xaxisCategories.add(lastMonth.format(monthYearFormatter));
			// init first & last date of month
			LocalDate datelastMonth =
					LocalDate.of(lastMonth.getYear(), lastMonth.getMonthValue(), 1);
			LocalDate firstDaylastMonth = datelastMonth.with(TemporalAdjusters.firstDayOfMonth());
			LocalDate lastDaylastMonth = datelastMonth.with(TemporalAdjusters.lastDayOfMonth());

			// setting count value
			seriesAppliedLoans.add(countLoanByParams(qLoan, null, branchIds, owners,
					firstDaylastMonth, lastDaylastMonth).intValue());
			seriesApprovedLoans.add(countLoanByParams(qLoan, listStatutApproved, branchIds, owners,
					firstDaylastMonth, lastDaylastMonth).intValue());
			seriesCanceledRejectedLoans.add(countLoanByParams(qLoan, listStatutRejectedCanceled,
					branchIds, owners, firstDaylastMonth, lastDaylastMonth).intValue());
		}

		// init current month DATA
		xaxisCategories.add(thisMonth.format(monthYearFormatter));
		// init first & last date of month
		LocalDate datethisMonth = LocalDate.of(thisMonth.getYear(), thisMonth.getMonthValue(), 1);
		LocalDate firstDaythisMonth = datethisMonth.with(TemporalAdjusters.firstDayOfMonth());
		LocalDate lastDaythisMonth = datethisMonth.with(TemporalAdjusters.lastDayOfMonth());

		// setting count value
		seriesAppliedLoans.add(countLoanByParams(qLoan, null, branchIds, owners, firstDaythisMonth,
				lastDaythisMonth).intValue());
		seriesApprovedLoans.add(countLoanByParams(qLoan, listStatutApproved, branchIds, owners,
				firstDaythisMonth, lastDaythisMonth).intValue());
		seriesCanceledRejectedLoans.add(countLoanByParams(qLoan, listStatutRejectedCanceled,
				branchIds, owners, firstDaythisMonth, lastDaythisMonth).intValue());

		// returning DATA
		return new LoanAnalyticsDTO(xaxisCategories, seriesAppliedLoans, seriesApprovedLoans,
				seriesCanceledRejectedLoans);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#totalLoansAmount()
	 */
	@Override
	public LoanAnalyticsDTO totalLoansAmount() {

		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// init List BranchIds
		List<Integer> branchIds = initListBranchIds(connectedUser);
		// init owners
		List<UserDTO> userDTOs = userClient.findUsers();
		List<String> owners = initListOwners(userDTOs);

		// init QLoan
		QLoan qLoan = QLoan.loan;

		// find by status
		List<Integer> status = Arrays
				.asList(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey(),
						CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE)
								.getKey());

		// build Predicate using given params
		BooleanBuilder predicate =
				preparePredicate(connectedUser, branchIds, owners, userDTOs, qLoan);

		// find only enabled data
		predicate.and(qLoan.enabled.eq(Boolean.TRUE));
		// find by given status
		predicate.and(qLoan.statutWorkflow.in(status));
		logger.info("PREDICATE = {}", predicate);
		// load total amount
		JPAQueryFactory query = new JPAQueryFactory(entityManager);
		BigDecimal total = query.select(qLoan.approvelAmount.sum().as("somme")).from(qLoan)
				.where(predicate).fetchFirst();
		// load currency from config
		AcmEnvironnementDTO acmEnvironnementDTO = parametrageClient.find("CURRENCY");
		LoanAnalyticsDTO loanAnalyticsDTO =
				new LoanAnalyticsDTO(total != null ? total.floatValue() : 0,
						acmEnvironnementDTO != null ? acmEnvironnementDTO.getValue() : "EGP");
		logger.info("TOTAL LOANS AMOUNT = {}", loanAnalyticsDTO);
		return loanAnalyticsDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#totalCustomers()
	 */
	@Override
	public LoanAnalyticsDTO totalCustomers() {

		// find TOTAL customer for connected user
		List<CustomerDTO> customerDTOs = customerService.findCustomers();
		logger.info("customerDTOs size = {}", customerDTOs.size());
		// Setting & returning data
		LoanAnalyticsDTO loanAnalyticsDTO = new LoanAnalyticsDTO();
		loanAnalyticsDTO.setTotalCustomers(customerDTOs.size());
		Float totalCustomers = new Float(customerDTOs.size());
		loanAnalyticsDTO.setPourcentage(
				CommonFunctions.calculateAndFormatPourcentage(totalCustomers, totalCustomers, 1));
		logger.info("TOTAL CUSTOMERS = {}", loanAnalyticsDTO);
		return loanAnalyticsDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#totalActiveCustomers()
	 */
	@Override
	public LoanAnalyticsDTO totalActiveCustomers() {

		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// init List BranchIds
		List<Integer> branchIds = initListBranchIds(connectedUser);
		// find loan not in status
		List<Integer> status = Arrays.asList(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey());
		// find TOTAL customer for connected user
		List<CustomerDTO> customerDTOs = customerService.findCustomers();
		logger.info("customerDTOs size = {}", customerDTOs.size());
		Float totalCustomers = new Float(customerDTOs.size());
		// init owners
		List<UserDTO> userDTOs = userClient.findUsers();
		List<String> owners = initListOwners(userDTOs);
		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate =
				preparePredicate(connectedUser, branchIds, owners, userDTOs, qLoan);

		// find only enabled data
		predicate.and(qLoan.enabled.eq(Boolean.TRUE));
		// find by given status
		predicate.and(qLoan.statutWorkflow.notIn(status));
		logger.info("PREDICATE = {}", predicate);
		Iterable<Loan> iterable = loanRepository.findAll(predicate);
		List<Loan> loans = new ArrayList<>();
		iterable.forEach(loans::add);
		// remove duplicate customer ID if exist
		Set<Loan> uniqueLoanSet = loans.stream() // get stream for original list
				.collect(Collectors.toCollection(// distinct elements stored into new SET
						() -> new TreeSet<>(Comparator.comparing(Loan::getCustomerId))));
		// setting & returning data
		LoanAnalyticsDTO loanAnalyticsDTO = new LoanAnalyticsDTO();
		Long totalActiveCustomer = Long.valueOf(uniqueLoanSet.size());
		loanAnalyticsDTO.setTotalActivesCustomers(totalActiveCustomer);
		loanAnalyticsDTO.setPourcentage(CommonFunctions.calculateAndFormatPourcentage(
				totalActiveCustomer.floatValue(), totalCustomers, 1));
		logger.info("TOTAL ACTIVE CUSTOMERS = {}", loanAnalyticsDTO);
		return loanAnalyticsDTO;
	}

	/**
	 * Prepare predicate By Connected User.
	 * 
	 * @author HaythemBenizid
	 * @param connectedUser the connected user
	 * @param branchIds the branch ids
	 * @param owners the owners
	 * @param userDTOs the user DT os
	 * @param qLoan the q loan
	 * @return the boolean builder
	 */
	private BooleanBuilder preparePredicate(UserDTO connectedUser, List<Integer> branchIds,
			List<String> owners, List<UserDTO> userDTOs, QLoan qLoan) {

		BooleanBuilder predicate = new BooleanBuilder();
		if (connectedUser.getCategory() != null
				&& connectedUser.getCategory().equals(UserCategory.MANAGMENT.name())) {
			// find by branch id IN OR owners IN
			BooleanBuilder subPredicate = new BooleanBuilder();
			subPredicate.and(qLoan.owner.in(owners));
			subPredicate.or(qLoan.branchID.in(branchIds));
			predicate.and(subPredicate);
		}
		else {
			BooleanBuilder subPredicate = new BooleanBuilder();
			// load loan by branch ID for SUPERVISOR
			boolean isManagerBranch = userDTOs.stream().anyMatch(
					user -> user.getTypeUser().equals(UserHierarchicalType.COLLABORATORS.name()));
			if (isManagerBranch) {
				// setting predicate to find by given branch Id
				subPredicate.and(qLoan.branchID.eq(connectedUser.getBranchID()));
				predicate.or(subPredicate);
			}
			// setting subPredicate to filter list participant by Id & status
			QLoanParticipants qLoanParticipants = QLoanParticipants.loanParticipants;
			List<LoanParticipantsDTO> loanParticipantsDTOs = loanParticipantsService
					.find(new LoanParticipantsDTO(null, connectedUser.getLogin()));
			if (!ACMValidationUtils.isNullOrEmpty(loanParticipantsDTOs)
					&& loanParticipantsDTOs.size() <= 1000) {
				List<Long> wheresIds = new ArrayList<>();
				loanParticipantsDTOs.forEach(
						loanParticipantsDTO -> wheresIds.add(loanParticipantsDTO.getIdLoan()));
				subPredicate.or(qLoan.idLoan.in(new ArrayList<>(new HashSet<>(wheresIds))));
				predicate.or(subPredicate);
			}
			else if (!ACMValidationUtils.isNullOrEmpty(loanParticipantsDTOs)
					&& loanParticipantsDTOs.size() > 1000) {
				subPredicate.or(qLoan.idLoan.in(JPAExpressions.selectFrom(qLoanParticipants)
						.select(qLoanParticipants.idLoan)
						.where(qLoanParticipants.username.eq(connectedUser.getLogin()))));
				predicate.or(subPredicate);
			}
		}
		return predicate;
	}

	/**
	 * Count loan by given params.
	 *
	 * @author HaythemBenizid
	 * @param qLoan the q loan
	 * @param listStatutWorkflow the list statut workflow
	 * @param branchIds the branch ids
	 * @param owners the owners
	 * @param monthStartDate the month start date
	 * @param monthEndDate the month end date
	 * @return the long
	 */
	private Long countLoanByParams(QLoan qLoan, List<Integer> listStatutWorkflow,
			List<Integer> branchIds, List<String> owners, LocalDate monthStartDate,
			LocalDate monthEndDate) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only Enabled data
		predicate.and(qLoan.enabled.eq(Boolean.TRUE));

		// find by Connected user : loan branchID OR loan owner
		predicate.and(qLoan.branchID.in(branchIds).or(qLoan.owner.in(owners)));

		// find by Status if exist
		if (!ACMValidationUtils.isNullOrEmpty(listStatutWorkflow)) {
			predicate.and(qLoan.statutWorkflow.in(listStatutWorkflow));
		}

		// find by ApplyDate for given month
		java.sql.Date sqlDateMin = java.sql.Date.valueOf(monthStartDate);
		java.sql.Date sqlDateMax = java.sql.Date.valueOf(monthEndDate);
		predicate.and(qLoan.applyDate.goe(sqlDateMin));
		predicate.and(qLoan.applyDate.loe(sqlDateMax));
		// find & return data
		return loanRepository.count(predicate);
	}

	/**
	 * Init list of access user branch && returning list of {@link Integer} branchIDs.
	 * 
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the list
	 */
	private List<Integer> initListBranchIds(UserDTO userDTO) {

		List<Integer> listBranchIds = new ArrayList<>();
		// add connected user branch Id
		listBranchIds.add(userDTO.getBranchID());
		// add Access Branches for connected user if exist
		if (!ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(",")).stream()
					.map(String::trim).mapToInt(Integer::parseInt).toArray();
			for (int i : arrayBranchIds) {
				listBranchIds.add(Integer.valueOf(i));
			}
		}
		return listBranchIds;
	}

	/**
	 * Inits the list owners : find all user RESPONSABLE && COLLABORATOR.
	 *
	 * @author HaythemBenizid
	 * @param userDTOs the user DT os
	 * @return the list
	 */
	private List<String> initListOwners(List<UserDTO> userDTOs) {

		List<String> wheresOwners = new ArrayList<>();
		userDTOs.forEach(user -> {
			if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
				wheresOwners.add(user.getLogin());
			}
		});
		logger.info("Method initListOwners() : List  RESPONSABLE && COLLABORATOR = {}",
				wheresOwners);
		return new ArrayList<>(new HashSet<>(wheresOwners));
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#loansAmountStatByMonths()
	 */
	@Override
	public LoanAnalyticsDTO loansAmountStatByMonths() {

		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// init List BranchIds
		List<Integer> branchIds = initListBranchIds(connectedUser);
		// init owners
		List<UserDTO> userDTOs = userClient.findUsers();
		List<String> owners = initListOwners(userDTOs);
		// init issued status
		List<Integer> status = Arrays
				.asList(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey(),
						CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE)
								.getKey());

		// init list
		List<String> xaxisCategories = new ArrayList<>();
		List<Long> seriesAmountLoans = new ArrayList<>();
		// init QLoan
		QLoan qLoan = QLoan.loan;
		// init format data
		YearMonth thisMonth = YearMonth.now();
		DateTimeFormatter monthYearFormatter = DateTimeFormatter.ofPattern("MMM yyyy");
		// init last 11 months DATA
		for (int i = 11; i > 0; i--) {
			// init Predicate
			BooleanBuilder predicate =
					preparePredicate(connectedUser, branchIds, owners, userDTOs, qLoan);
			// find only Enabled data
			predicate.and(qLoan.enabled.eq(Boolean.TRUE));
			// find by Status not IN
			predicate.and(qLoan.statutWorkflow.in(status));
			YearMonth lastMonth = thisMonth.minusMonths(i);
			// setting month label
			xaxisCategories.add(lastMonth.format(monthYearFormatter));
			// init first & last date of month
			LocalDate datelastMonth =
					LocalDate.of(lastMonth.getYear(), lastMonth.getMonthValue(), 1);
			LocalDate firstDaylastMonth = datelastMonth.with(TemporalAdjusters.firstDayOfMonth());
			LocalDate lastDaylastMonth = datelastMonth.with(TemporalAdjusters.lastDayOfMonth());
			// find by ApplyDate for given month
			java.sql.Date sqlDateMin = java.sql.Date.valueOf(firstDaylastMonth);
			java.sql.Date sqlDateMax = java.sql.Date.valueOf(lastDaylastMonth);
			predicate.and(qLoan.applyDate.goe(sqlDateMin));
			predicate.and(qLoan.applyDate.loe(sqlDateMax));

			// calculate
			JPAQueryFactory query = new JPAQueryFactory(entityManager);
			BigDecimal total = query.select(qLoan.approvelAmount.sum().as("somme")).from(qLoan)
					.where(predicate).fetchFirst();
			// setting loan amount by month
			seriesAmountLoans.add(total != null ? total.longValue() : 0);
		}

		// init Predicate
		BooleanBuilder predicate =
				preparePredicate(connectedUser, branchIds, owners, userDTOs, qLoan);
		// find only Enabled data
		predicate.and(qLoan.enabled.eq(Boolean.TRUE));
		// find by Status not IN
		predicate.and(qLoan.statutWorkflow.in(status));
		// init current month DATA
		xaxisCategories.add(thisMonth.format(monthYearFormatter));
		// init first & last date of month
		LocalDate datethisMonth = LocalDate.of(thisMonth.getYear(), thisMonth.getMonthValue(), 1);
		LocalDate firstDaythisMonth = datethisMonth.with(TemporalAdjusters.firstDayOfMonth());
		LocalDate lastDaythisMonth = datethisMonth.with(TemporalAdjusters.lastDayOfMonth());
		// find by ApplyDate for given month
		java.sql.Date sqlDateMin = java.sql.Date.valueOf(firstDaythisMonth);
		java.sql.Date sqlDateMax = java.sql.Date.valueOf(lastDaythisMonth);
		predicate.and(qLoan.applyDate.goe(sqlDateMin));
		predicate.and(qLoan.applyDate.loe(sqlDateMax));

		// calculate amount this Month
		JPAQueryFactory query = new JPAQueryFactory(entityManager);
		BigDecimal totalthisMonth = query.select(qLoan.approvelAmount.sum().as("somme")).from(qLoan)
				.where(predicate).fetchFirst();
		// setting loan amount for this month
		seriesAmountLoans.add(totalthisMonth != null ? totalthisMonth.longValue() : 0);

		// returning DATA
		LoanAnalyticsDTO loanAnalyticsDTO = new LoanAnalyticsDTO();
		loanAnalyticsDTO.setXaxisCategories(xaxisCategories);
		loanAnalyticsDTO.setSeriesAmountLoans(seriesAmountLoans);
		logger.info("LOAN AMOUNT BY MONTH = {}", loanAnalyticsDTO);
		return loanAnalyticsDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AnalyticsService#customersStatByMonths()
	 */
	@Override
	public LoanAnalyticsDTO customersStatByMonths() {

		// init list
		List<String> xaxisCategories = new ArrayList<>();
		List<Integer> seriesTotalCustomers = new ArrayList<>();
		List<Integer> seriesTotalActiveCustomers = new ArrayList<>();

		YearMonth thisMonth = YearMonth.now();
		DateTimeFormatter monthYearFormatter = DateTimeFormatter.ofPattern("MMM yyyy");
		// init last 11 months DATA
		for (int i = 11; i > 0; i--) {
			YearMonth lastMonth = thisMonth.minusMonths(i);
			// setting month label
			xaxisCategories.add(lastMonth.format(monthYearFormatter));
			// init first & last date of month
			LocalDate datelastMonth =
					LocalDate.of(lastMonth.getYear(), lastMonth.getMonthValue(), 1);
			LocalDate firstDaylastMonth = datelastMonth.with(TemporalAdjusters.firstDayOfMonth());
			LocalDate lastDaylastMonth = datelastMonth.with(TemporalAdjusters.lastDayOfMonth());

			// setting count value
			Map<Integer, Integer> resulatMap =
					customerService.findForAnalytics(null, firstDaylastMonth, lastDaylastMonth);
			Map.Entry<Integer, Integer> entry = resulatMap.entrySet().iterator().next();
			seriesTotalCustomers.add(entry.getKey());
			seriesTotalActiveCustomers.add(entry.getValue());
		}

		// init current month DATA
		xaxisCategories.add(thisMonth.format(monthYearFormatter));
		// init first & last date of month
		LocalDate datethisMonth = LocalDate.of(thisMonth.getYear(), thisMonth.getMonthValue(), 1);
		LocalDate firstDaythisMonth = datethisMonth.with(TemporalAdjusters.firstDayOfMonth());
		LocalDate lastDaythisMonth = datethisMonth.with(TemporalAdjusters.lastDayOfMonth());

		// setting count value
		Map<Integer, Integer> resulatMap =
				customerService.findForAnalytics(null, firstDaythisMonth, lastDaythisMonth);
		Map.Entry<Integer, Integer> entry = resulatMap.entrySet().iterator().next();
		seriesTotalCustomers.add(entry.getKey());
		seriesTotalActiveCustomers.add(entry.getValue());
		// returning DATA
		LoanAnalyticsDTO loanAnalyticsDTO = new LoanAnalyticsDTO();
		loanAnalyticsDTO.setXaxisCategories(xaxisCategories);
		loanAnalyticsDTO.setSeriesTotalCustomers(seriesTotalCustomers);
		loanAnalyticsDTO.setSeriesTotalActiveCustomers(seriesTotalActiveCustomers);
		logger.info("LOAN AMOUNT BY MONTH = {}", loanAnalyticsDTO);
		return loanAnalyticsDTO;
	}
}
