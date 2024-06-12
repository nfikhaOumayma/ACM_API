/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.dozer.DozerBeanMapper;
import org.json.JSONArray;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonLoggerMessage;
import com.acm.model.rowmapper.CustomerActiveAccountAbacusRowMapper;
import com.acm.model.rowmapper.LoanAbacusRowMapper;
import com.acm.model.rowmapper.LoanApprovalProcessAbacusRowMapper;
import com.acm.model.rowmapper.LoanCanceledAbacusRowMapper;
import com.acm.model.rowmapper.LoanDTOAbacusRowMapper;
import com.acm.model.rowmapper.ScheduleDTOAbacusRowMapper;
import com.acm.model.rowmapper.SettingMotifRejetsDTORowMapper;
import com.acm.model.rowmapper.SettingTopupValidityRowMapper;
import com.acm.service.LoanAbacusService;
import com.acm.utils.dtos.LoanApprovalProcessDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.SettingMotifRejetsDTO;
import com.acm.utils.dtos.SettingTopupDTO;
import com.acm.utils.dtos.SettingTopupValidityDTO;
import com.acm.utils.models.Loan;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link LoanAbacusServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Service
public class LoanAbacusServiceImpl implements LoanAbacusService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanAbacusServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#find(java.lang.Long)
	 */
	@Override
	public List<LoanDTO> find(Long limite) {

		try {
			// load query
			String query = environment.getProperty("query.select.from.culoan.where.statut");
			logger.info("findLoan : limite= {}", limite);
			// init params
			SqlParameterSource namedParameters = new MapSqlParameterSource()
					.addValue("statut", Integer.valueOf(1)).addValue("limite", limite);
			// execute query
			List<Loan> result = namedParameterJdbcTemplate.query(query, namedParameters,
					new LoanAbacusRowMapper());
			// mapping data if list not empty
			List<LoanDTO> loanDTOs = new ArrayList<>();
			if (!ACMValidationUtils.isNullOrEmpty(result)) {
				result.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
			}
			logger.info(" find LOAN data from ABACUS DB :: DONE");
			// returning sorted list by IdLoanExtern
			return loanDTOs.stream().sorted(Comparator.comparingLong(LoanDTO::getIdLoanExtern))
					.collect(Collectors.toList());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#findLoanByAccountNumber(java.lang.String)
	 */
	@Override
	public List<LoanDTO> findLoanByAccountNumber(String accountNumber) {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.by.account.number");
			logger.info("findLoan by account number: accountNumber= {}", accountNumber);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("statut", Integer.valueOf(1))
							.addValue("accountNumber", accountNumber);
			// execute query
			List<Loan> result = namedParameterJdbcTemplate.query(query, namedParameters,
					new LoanAbacusRowMapper());
			// mapping data if list not empty
			List<LoanDTO> loanDTOs = new ArrayList<>();
			if (!ACMValidationUtils.isNullOrEmpty(result)) {
				result.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
			}
			logger.info(" find LOAN data from ABACUS DB :: DONE");
			// returning sorted list by IdLoanExtern
			return loanDTOs.stream().sorted(Comparator.comparingLong(LoanDTO::getIdLoanExtern))
					.collect(Collectors.toList());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#findIssuedLoans(java.util.List)
	 */
	@Override
	public List<LoanDTO> findIssuedLoans(List<Long> ids) {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.status.issued");
			logger.info("findIssuedLoans in given list of ids = {}", ids);
			// init paramsfindIssuedLoans
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("list_id_loan", ids);

			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new RowMapper<LoanDTO>() {
						@Override
						public LoanDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

							LoanDTO loanDTO = new LoanDTO();
							loanDTO.setIdLoanExtern(rs.getLong("CULoanID"));
							loanDTO.setProductId(rs.getInt("productID"));
							loanDTO.setCustomerId(rs.getLong("customerID"));
							loanDTO.setIssueDate(rs.getDate("IssueDate"));
							return loanDTO;
						}
					});
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.loan.status.issued### {}",
					e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#findAllMotifRejet()
	 */
	@Override
	public List<SettingMotifRejetsDTO> findAllMotifRejet() {

		try {
			// load query
			String query = environment.getProperty("query.select.all.abacus.cancelReasons");
			logger.info("findmotifsRejet");

			// execute query
			return namedParameterJdbcTemplate.query(query, new SettingMotifRejetsDTORowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.all.abacus.cancelReasons### {}",
					e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#findDetails(java.lang.Long)
	 */
	@Override
	public LoanDTO findDetails(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.details");
			logger.info("findLoan by idloan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_loan", idLoan);
			// execute query
			return namedParameterJdbcTemplate.queryForObject(query, namedParameters,
					new LoanDTOAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new LoanDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#findSchedule(java.lang.Long)
	 */
	@Override
	public List<ScheduleDTO> findSchedule(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.Schedule");
			logger.info("findSchedule by idloan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_loan", idLoan);
			// execute query
			List<ScheduleDTO> scheduleDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new ScheduleDTOAbacusRowMapper());

			BigDecimal totalRepayment = BigDecimal.ZERO;
			BigDecimal totalLoanRepayment = BigDecimal.ZERO;
			BigDecimal totalInterestRepayment = BigDecimal.ZERO;
			for (ScheduleDTO scheduleDTO : scheduleDTOs) {
				totalRepayment = totalRepayment.add(scheduleDTO.getTotalRepayment());
				totalLoanRepayment = totalLoanRepayment.add(scheduleDTO.getLoanRepayment());
				totalInterestRepayment =
						totalInterestRepayment.add(scheduleDTO.getInterestRepayment());
			}
			// setting Total
			scheduleDTOs.add(
					new ScheduleDTO(totalRepayment, totalLoanRepayment, totalInterestRepayment));
			return scheduleDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#findApprovalProcess(java.lang.Long)
	 */
	@Override
	public List<LoanApprovalProcessDTO> findApprovalProcess(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.process");
			logger.info("findApprovalProcess by idloan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_loan", idLoan);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new LoanApprovalProcessAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#findCanceledLoan()
	 */
	@Override
	public List<LoanDTO> findCanceledLoan() {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.status.canceled");

			// execute query
			return namedParameterJdbcTemplate.query(query, new LoanCanceledAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#findActiveAccountForCustomerAndLoan(java.lang.Long,
	 * java.lang.Long)
	 */
	@Override
	public Long findActiveAccountForCustomerAndLoan(List<Long> idAccounts) {

		try {
			// load query
			String query = environment.getProperty("query.select.active.account.customer.loan");
			logger.info("findActiveAccountForCustomerAndLoan : idAccount= {}", idAccounts);

			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_account", idAccounts);
			// execute query
			return namedParameterJdbcTemplate.queryForObject(query, namedParameters,
					new CustomerActiveAccountAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.active.account.customer.loan ### {}",
					e.getMessage());
		}
		return 0L;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#getClosingBalancebyIdLoan(java.lang.Long)
	 */
	@Override
	public Long getClosingBalancebyIdLoan(Long idLoanExtern) {

		try {
			// load query
			String query = environment.getProperty("query.select.closing.balance.by.idloan");
			logger.info("getClosingBalancebyIdLoan : idLoanExtern= {}", idLoanExtern);

			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_loan", idLoanExtern);
			// execute query
			return namedParameterJdbcTemplate.queryForObject(query, namedParameters,
					new RowMapper<Long>() {
						@Override
						public Long mapRow(ResultSet rs, int rowNum) throws SQLException {

							return rs.getLong("BALANCE");
						}
					});
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.closing.balance.by.idloan ### {}",
					e.getMessage());
		}
		return 0L;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanAbacusService#checkSettingTopupValidity(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public SettingTopupValidityDTO checkSettingTopupValidity(LoanDTO loanDTO) {

		List<SettingTopupDTO> settingTopupDTOs = loanDTO.getSettingTopupDTOs();

		SettingTopupValidityDTO settingTopupValidityDTO = new SettingTopupValidityDTO();
		try {
			// prepare json to be sent to the query
			JSONObject jsonParam = new JSONObject();
			JSONArray arrayParam = new JSONArray();
			for (SettingTopupDTO settingTopup : settingTopupDTOs) {
				jsonParam.put("ProductId", settingTopup.getProductAbacusId());
				jsonParam.put("MinBr4", settingTopup.getTopupMinPreviouslyIssuedLoansNumber());
				jsonParam.put("MinBr0", settingTopup.getTopupMinLoanPaymentPercentage());
				jsonParam.put("MaxContinuousLateDaysBr1",
						settingTopup.getTopupMaxContinuousLateDays());
				jsonParam.put("MaxSeparateLateDaysBr1", settingTopup.getTopupMaxSeparateLateDays());
				arrayParam.put(jsonParam);
			}
			// load query

			String query = environment.getProperty("query.select.setting.topup.validity");
			logger.info(" checkSettingTopupValidity of idloan= {}", loanDTO.getIdLoanExtern());
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("id_loan", loanDTO.getIdLoanExtern())
							.addValue("id_customer", loanDTO.getCustomerId())
							.addValue("product_id", loanDTO.getProductDTO().getProductIdAbacus())
							.addValue("json", arrayParam.toString());
			// execute query
			return namedParameterJdbcTemplate.queryForObject(query, namedParameters,
					new SettingTopupValidityRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return settingTopupValidityDTO;

	}
}
