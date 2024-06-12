package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.repository.AcmCreditLineRepository;
import com.acm.repository.AcmToppedUpHistoryRepository;
import com.acm.service.CreditLineService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmCreditLineDTO;
import com.acm.utils.dtos.AcmCreditLinePaginationDTO;
import com.acm.utils.dtos.AcmToppedUpHistoryDTO;
import com.acm.utils.models.AcmCreditLine;
import com.acm.utils.models.AcmToppedUpHistory;
import com.acm.utils.models.QAcmCreditLine;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class CreditLineServiceImpl.
 */
@Service
public class CreditLineServiceImpl implements CreditLineService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CreditLineServiceImpl.class);
	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The acm credit line repository. */
	@Autowired
	AcmCreditLineRepository acmCreditLineRepository;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The acm topped up history repository. */
	@Autowired
	private AcmToppedUpHistoryRepository acmToppedUpHistoryRepository;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CreditLineService#save(com.acm.utils.dtos.AcmCreditLineDTO)
	 */
	@Override
	public AcmCreditLineDTO save(AcmCreditLineDTO acmCreditLineDTO) {

		AcmCreditLine acmCreditLine = mapper.map(acmCreditLineDTO, AcmCreditLine.class);
		CommonFunctions.mapperToSave(acmCreditLine, userClient, logger);

		// save credit line + topped up histories
		for (AcmToppedUpHistory toppedUpHistory : acmCreditLine.getToppedUpHistories()) {

			toppedUpHistory.setCreditLine(acmCreditLine);
		}

		acmCreditLine = acmCreditLineRepository.save(acmCreditLine);

		// set creditLine null (lazy load)
		for (AcmToppedUpHistory toppedUpHistory : acmCreditLine.getToppedUpHistories()) {
			toppedUpHistory.setCreditLine(null);
		}

		AcmCreditLineDTO acmCreditLineDTONew = mapper.map(acmCreditLine, AcmCreditLineDTO.class);

		logger.info("Save CREDIT LINE successfully");
		return acmCreditLineDTONew;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CreditLineService#find(com.acm.utils.dtos.AcmCreditLinePaginationDTO)
	 */
	@Override
	public AcmCreditLinePaginationDTO find(AcmCreditLinePaginationDTO acmCreditLinePaginationDTO) {

		Preconditions.checkNotNull(acmCreditLinePaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Initialize default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(acmCreditLinePaginationDTO.getPageNumber())) {
			acmCreditLinePaginationDTO.setPageNumber(0);
		}
		// Initialize default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(acmCreditLinePaginationDTO.getPageSize())) {
			acmCreditLinePaginationDTO.setPageSize(10);
		}
		// setting default data
		acmCreditLinePaginationDTO.setResultsCreditLine(new ArrayList<>());
		// setting default totals pages
		acmCreditLinePaginationDTO.setTotalElements(0L);
		// setting default totals elements
		acmCreditLinePaginationDTO.setTotalPages(0);

		// Initialize QAcmCreditLine
		QAcmCreditLine qacmCreditLine = QAcmCreditLine.acmCreditLine;
		// Initialize Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by id
		if (!ACMValidationUtils.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getId())) {
			predicate.and(qacmCreditLine.id.eq(acmCreditLinePaginationDTO.getParams().getId()));
		}

		// find LIKE fundName
		if (!ACMValidationUtils
				.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getFundName())) {
			predicate.and(qacmCreditLine.fundName
					.like("%" + acmCreditLinePaginationDTO.getParams().getFundName() + "%"));
		}

		// find LIKE description
		if (!ACMValidationUtils
				.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getDescription())) {
			predicate.and(qacmCreditLine.description
					.like("%" + acmCreditLinePaginationDTO.getParams().getDescription() + "%"));
		}

		// find LIKE balance
		if (!ACMValidationUtils
				.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getBalance())) {
			predicate.and(qacmCreditLine.balance
					.like("%" + acmCreditLinePaginationDTO.getParams().getBalance() + "%"));
		}

		// find LIKE fundPriority
		if (!ACMValidationUtils
				.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getFundPriority())) {
			predicate.and(qacmCreditLine.fundPriority
					.like("%" + acmCreditLinePaginationDTO.getParams().getFundPriority() + "%"));
		}

		// find LIKE issueDate
		if (!ACMValidationUtils
				.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getIssueDate())) {
			java.sql.Date sqlDate = java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(
					acmCreditLinePaginationDTO.getParams().getIssueDate()));
			predicate.and(qacmCreditLine.issueDate.eq(sqlDate));
		}

		// find LIKE expiryDate
		if (!ACMValidationUtils
				.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getExpiryDate())) {
			java.sql.Date sqlDate = java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(
					acmCreditLinePaginationDTO.getParams().getExpiryDate()));
			predicate.and(qacmCreditLine.expiryDate.eq(sqlDate));
		}
		// find LIKE thirdparty
		if (!ACMValidationUtils
				.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getThirdParty())) {
			if (!ACMValidationUtils.isNullOrEmpty(
					acmCreditLinePaginationDTO.getParams().getThirdParty().getFirstName())) {
				predicate.and(qacmCreditLine.thirdParty.firstName.like(
						"%" + acmCreditLinePaginationDTO.getParams().getThirdParty().getFirstName()
								+ "%"));
			}
		}

		// find by enabled
		if (!ACMValidationUtils
				.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getControlBalance())) {
			predicate.and(qacmCreditLine.controlBalance
					.eq(acmCreditLinePaginationDTO.getParams().getControlBalance()));
		}

		// find by enabled
		if (!ACMValidationUtils
				.isNullOrEmpty(acmCreditLinePaginationDTO.getParams().getEnabled())) {
			predicate.and(
					qacmCreditLine.enabled.eq(acmCreditLinePaginationDTO.getParams().getEnabled()));
		}
		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if (!ACMValidationUtils.isNullOrEmpty(acmCreditLinePaginationDTO.getSortField())) {
			String sortedField = acmCreditLinePaginationDTO.getSortField();
			if (acmCreditLinePaginationDTO.getSortField().equals("thirdParty")) {
				sortedField = "thirdParty";
			}
			if (acmCreditLinePaginationDTO.getSortField().equals("fundName")) {
				sortedField = "fundName";
			}
			if (acmCreditLinePaginationDTO.getSortField().equals("description")) {
				sortedField = "description";
			}
			if (acmCreditLinePaginationDTO.getSortField().equals("balance")) {
				sortedField = "balance";
			}
			if (acmCreditLinePaginationDTO.getSortField().equals("fundPriority")) {
				sortedField = "fundPriority";
			}
			if (acmCreditLinePaginationDTO.getSortField().equals("issueDate")) {
				sortedField = "issueDate";
			}
			if (acmCreditLinePaginationDTO.getSortField().equals("expiryDate")) {
				sortedField = "expiryDate";
			}
			if (acmCreditLinePaginationDTO.getSortField().equals("controlBalance")) {
				sortedField = "controlBalance";
			}
			if ("1".equals(acmCreditLinePaginationDTO.getSortDirection())) {
				pageable = PageRequest.of(acmCreditLinePaginationDTO.getPageNumber(),
						acmCreditLinePaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
			}
			else if ("-1".equals(acmCreditLinePaginationDTO.getSortDirection())) {
				pageable = PageRequest.of(acmCreditLinePaginationDTO.getPageNumber(),
						acmCreditLinePaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
			}
			else {
				// default sort
				pageable = PageRequest.of(acmCreditLinePaginationDTO.getPageNumber(),
						acmCreditLinePaginationDTO.getPageSize(), Sort.Direction.ASC,
						"dateInsertion");
			}
		}
		else {
			// default sort
			pageable = PageRequest.of(acmCreditLinePaginationDTO.getPageNumber(),
					acmCreditLinePaginationDTO.getPageSize(), Sort.Direction.ASC, "dateInsertion");
		}

		// load data
		Page<AcmCreditLine> pagedResult = acmCreditLineRepository.findAll(predicate, pageable);

		if (pagedResult.hasContent()) {
			List<AcmCreditLine> acmCreditLineList = pagedResult.getContent();
			logger.info("{} : User was founded (PageNumber = {} / PageSize = {} )",
					acmCreditLineList.size(), acmCreditLinePaginationDTO.getPageNumber(),
					acmCreditLinePaginationDTO.getPageSize());
			List<AcmCreditLineDTO> creditLineDTOs = new ArrayList<>();
			// set creditLine null (lazy load)
			acmCreditLineList.forEach(creditLine -> {
				creditLine.getToppedUpHistories()
						.forEach(toppedUpHistory -> toppedUpHistory.setCreditLine(null));
			});
			// get only enabled topped up history
			acmCreditLineList.forEach(creditLine -> {
				Set<AcmToppedUpHistory> enabledHistories = creditLine.getToppedUpHistories()
						.stream().filter(history -> history.getEnabled().equals(Boolean.TRUE))
						.collect(Collectors.toSet());
				creditLine.setToppedUpHistories(enabledHistories);
			});

			acmCreditLineList.forEach(creditLine -> creditLineDTOs
					.add(mapper.map(creditLine, AcmCreditLineDTO.class)));
			// setting data
			acmCreditLinePaginationDTO.setResultsCreditLine(creditLineDTOs);
			// setting totals pages
			acmCreditLinePaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			acmCreditLinePaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		logger.info("Executing Method UserPagination() :: DONE");

		return acmCreditLinePaginationDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CreditLineService#deleteToppedUpHistories(java.util.List)
	 */
	@Override
	public void deleteToppedUpHistories(List<AcmToppedUpHistoryDTO> toppedUpHistories) {

		toppedUpHistories.forEach(toppedUpHistory -> {
			toppedUpHistory.setEnabled(Boolean.FALSE);
		});

		List<AcmToppedUpHistory> toppedUpHistoriesToRemove = new ArrayList<AcmToppedUpHistory>();

		toppedUpHistories.forEach(toppedUpHistory -> {
			toppedUpHistoriesToRemove.add(mapper.map(toppedUpHistory, AcmToppedUpHistory.class));
		});

		acmToppedUpHistoryRepository.saveAll(toppedUpHistoriesToRemove);

		logger.info("Topped Up Histories disabled", toppedUpHistoriesToRemove.size());
	}

}
