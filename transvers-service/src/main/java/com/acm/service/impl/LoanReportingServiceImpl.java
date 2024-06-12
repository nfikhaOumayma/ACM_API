/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonLoggerMessage;
import com.acm.model.rowmapper.ReportingSchedulesStatusDTOAbacusRowMapper;
import com.acm.service.LoanReportingService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingSchedulesStatusDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link LoanReportingServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.1.2
 */
@Service
public class LoanReportingServiceImpl implements LoanReportingService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanReportingServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The Constant WHERE_CLAUSE. */
	private static final String WHERE_CLAUSE = " WHERE ";

	/** The Constant AND_CLAUSE. */
	private static final String AND_CLAUSE = " AND ";

	/** The Constant POURCENTAGE_CLAUSE. */
	private static final String POURCENTAGE_CLAUSE = "%";

	/** The Constant STRING_CHAR_CLAUSE. */
	private static final String STRING_CHAR_CLAUSE = "'";

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanReportingService#findSchedulesStatus(com.acm.utils.dtos.ReportingDTO)
	 */
	@Override
	public List<ReportingSchedulesStatusDTO> findSchedulesStatus(ReportingDTO reportingDTO) {

		// SourceOffunds IN (1,2)
		final String SOURCEOFFUNDS_IN_CLAUSE = "SourceOffunds IN ";
		// Branche IN (1,4,5,6)
		final String BRANCHE_IN_CLAUSE = " Branche IN ";
		// LoanOfficer IN (1,2,3)
		final String LOANOFFICER_IN_CLAUSE = " LoanOfficer IN ";
		// ProductID IN (1,2,3,4,5)
		final String PRODUCTID_IN_CLAUSE = " ProductID IN ";
		// RepaymentDate BETWEEN '01/01/yyyy' AND '31/12/yyyy'
		final String REPAYMENTDATE_BETWEEN_CLAUSE = " CONVERT(datetime,REPAYMENTDATE,120) BETWEEN ";
		// LoanStatus IN (4,8)
		final String LOANSTATUS_IN_CLAUSE = " LoanStatus IN ";
		// CustomerNumber Like '%1%'
		final String CUSTOMERNUMBER_LIKE_CLAUSE = " CustomerNumber LIKE ";
		// GroupNumber LIKE '%1%'
		final String GROUPNUMBER_LIKE_CLAUSE = " GroupNumber LIKE ";

		try {
			// load query
			logger.info("Executing <<query.reporting.acm.schedules.status>> data from ABACUS DB");
			String query = environment.getProperty("query.reporting.acm.schedules.status");

			// RepaymentDate BETWEEN '01-01-yyyy' AND '31-12-yyyy'
			String dateInstalmentDateMin = reportingDTO.getInstalmentDateMin() != null
					? DateUtil.formatDate(reportingDTO.getInstalmentDateMin(), "yyyy-MM-dd")
					: DateUtil.getYearFromDate(new Date()) + "-01-01";
			String dateInstalmentDateMax = reportingDTO.getInstalmentDateMax() != null
					? DateUtil.formatDate(reportingDTO.getInstalmentDateMax(), "yyyy-MM-dd")
					: DateUtil.getYearFromDate(new Date()) + "-12-31";

			String clauseInstalmentDate = REPAYMENTDATE_BETWEEN_CLAUSE + "CONVERT(datetime, "
					+ STRING_CHAR_CLAUSE + dateInstalmentDateMin + STRING_CHAR_CLAUSE + ",120)"
					+ AND_CLAUSE + "CONVERT(datetime, " + STRING_CHAR_CLAUSE + dateInstalmentDateMax
					+ STRING_CHAR_CLAUSE + ",120)";
			logger.info("SQL : clauseInstalmentDate = {}", clauseInstalmentDate);
			String queryWhereClause = WHERE_CLAUSE + clauseInstalmentDate;

			// SourceOffunds IN (1,2)
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanSourceOfFundsDTOs())) {
				Integer[] array = new Integer[reportingDTO.getLoanSourceOfFundsDTOs().size()];
				// copy elements from list to int array
				for (int i = 0; i < array.length; i++) {
					array[i] = reportingDTO.getLoanSourceOfFundsDTOs().get(i)
							.getLoanSourceOfFundsID().intValue();
				}
				String ids = "(" + Arrays.toString(array) + ")";
				ids = ids.replace("[", "");
				ids = ids.replace("]", "");
				queryWhereClause = queryWhereClause + AND_CLAUSE + SOURCEOFFUNDS_IN_CLAUSE + ids;
				logger.info("SQL : SOURCEOFFUNDS_IN_CLAUSE = {}", queryWhereClause);
			}

			// Branche IN (1,4,5,6)
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getBrancheDTOs())) {
				Integer[] array = new Integer[reportingDTO.getBrancheDTOs().size()];
				// copy elements from list to int array
				for (int i = 0; i < array.length; i++) {
					array[i] = reportingDTO.getBrancheDTOs().get(i).getBranchID().intValue();
				}
				String ids = "(" + Arrays.toString(array) + ")";
				ids = ids.replace("[", "");
				ids = ids.replace("]", "");
				queryWhereClause = queryWhereClause + AND_CLAUSE + BRANCHE_IN_CLAUSE + ids;
				logger.info("SQL : BRANCHE_IN_CLAUSE = {}", queryWhereClause);
			}

			// LoanOfficer IN (1,2,3)
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getUserDTOs())) {
				Integer[] array = new Integer[reportingDTO.getUserDTOs().size()];
				// copy elements from list to int array
				for (int i = 0; i < array.length; i++) {
					array[i] = reportingDTO.getUserDTOs().get(i).getAccountPortfolioId().intValue();
				}
				String ids = "(" + Arrays.toString(array) + ")";
				ids = ids.replace("[", "");
				ids = ids.replace("]", "");
				queryWhereClause = queryWhereClause + AND_CLAUSE + LOANOFFICER_IN_CLAUSE + ids;
				logger.info("SQL : LOANOFFICER_IN_CLAUSE = {}", queryWhereClause);
			}

			// ProductID IN (1,2,3,4,5)
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getProductDTOs())) {
				Integer[] array = new Integer[reportingDTO.getProductDTOs().size()];
				// copy elements from list to int array
				for (int i = 0; i < array.length; i++) {
					array[i] = reportingDTO.getProductDTOs().get(i).getId().intValue();
				}
				String ids = "(" + Arrays.toString(array) + ")";
				ids = ids.replace("[", "");
				ids = ids.replace("]", "");
				queryWhereClause = queryWhereClause + AND_CLAUSE + PRODUCTID_IN_CLAUSE + ids;
				logger.info("SQL : PRODUCTID_IN_CLAUSE = {}", queryWhereClause);
			}

			// LoanStatus IN (4,8)
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanStatus())) {
				Integer[] array = new Integer[reportingDTO.getLoanStatus().size()];
				// copy elements from list to int array
				for (int i = 0; i < array.length; i++) {
					array[i] = reportingDTO.getLoanStatus().get(i).getKey();
				}
				String ids = "(" + Arrays.toString(array) + ")";
				ids = ids.replace("[", "");
				ids = ids.replace("]", "");
				queryWhereClause = queryWhereClause + AND_CLAUSE + LOANSTATUS_IN_CLAUSE + ids;
				logger.info("SQL : LOANSTATUS_IN_CLAUSE = {}", queryWhereClause);
			}

			// CustomerNumber Like '%1%'
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getCustomerNumber())) {
				String clauseCustomerNumber = CUSTOMERNUMBER_LIKE_CLAUSE + STRING_CHAR_CLAUSE
						+ POURCENTAGE_CLAUSE + reportingDTO.getCustomerNumber() + POURCENTAGE_CLAUSE
						+ STRING_CHAR_CLAUSE;
				logger.info("SQL : clauseCustomerNumber = {}", clauseCustomerNumber);
				queryWhereClause = queryWhereClause + AND_CLAUSE + clauseCustomerNumber;
			}

			// GroupNumber LIKE '%1%'
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getGroupNumber())) {
				String clauseGroupNumber = GROUPNUMBER_LIKE_CLAUSE + STRING_CHAR_CLAUSE
						+ POURCENTAGE_CLAUSE + reportingDTO.getGroupNumber() + POURCENTAGE_CLAUSE
						+ STRING_CHAR_CLAUSE;
				logger.info("SQL : clauseGroupNumber = {}", clauseGroupNumber);
				queryWhereClause = queryWhereClause + AND_CLAUSE + clauseGroupNumber;
			}

			logger.info("SQL : queryWhereClause = {}", queryWhereClause);
			// add where clause if exist
			if (!ACMValidationUtils.isNullOrEmpty(queryWhereClause)) {
				query = query + queryWhereClause;
			}

			// Result max size = 1000
			String queryRange =
					"ORDER BY ACCOUNTNUMBER ASC OFFSET 0 ROWS FETCH NEXT 1000 ROWS ONLY";
			query = query + queryRange;
			logger.info("SQL : QUERY = {}", query);
			// execute query
			List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOs =
					namedParameterJdbcTemplate.query(query,
							new ReportingSchedulesStatusDTOAbacusRowMapper());
			// returning result
			logger.info("reportingSchedulesStatusDTOs LIST size = {}",
					reportingSchedulesStatusDTOs.size());
			return reportingSchedulesStatusDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}
}
