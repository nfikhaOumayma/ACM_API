/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
import com.acm.model.rowmapper.ArrearsDTOAbacusRowMapper;
import com.acm.model.rowmapper.CustomerAccountDTOAbacusRowMapper;
import com.acm.model.rowmapper.CustomerActiveAccountAbacusRowMapper;
import com.acm.model.rowmapper.CustomerActiveAccountDTOAbacusRowMapper;
import com.acm.model.rowmapper.CustomerDTOAbacusRowMapper;
import com.acm.model.rowmapper.CustomerLoanDTOAbacusRowMapper;
import com.acm.model.rowmapper.CustomerMembersGRPAbacusRowMapper;
import com.acm.model.rowmapper.CustomerMembersORGAbacusRowMapper;
import com.acm.model.rowmapper.CustomerRelationshipAbacusRowMapper;
import com.acm.model.rowmapper.LoanDetailsInformationsRowMapper;
import com.acm.model.rowmapper.ScheduleAccountCustomerAbacusRowMapper;
import com.acm.service.CustomerAbacusService;
import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerMemberDTO;
import com.acm.utils.dtos.LoanDetailsInfoResponseDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link CustomerAbacusServiceImpl} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
@Service
public class CustomerAbacusServiceImpl implements CustomerAbacusService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(CustomerAbacusServiceImpl.class);

	/** The environment. */
	@Autowired
	private Environment environment;

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The id loan constant. */
	private static final String ID_LOAN_CONSTANT = "id_loan";

	/** The Constant ID_CUSTOMER_CONSTANT. */
	private static final String ID_CUSTOMER_CONSTANT = "id_customer";

	/** The Constant ID_PRODUCT_CONSTANT. */
	private static final String ID_PRODUCT_CONSTANT = "id_product";

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#find(java.lang.Long)
	 */
	@Override
	public CustomerDTO find(Long idCustomer) {

		try {
			// load query
			String query = environment.getProperty("query.select.custumer");
			logger.info("find by idCustomer= {}", idCustomer);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);
			// execute query
			List<CustomerDTO> customerDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new CustomerDTOAbacusRowMapper());

			return ACMValidationUtils.isNullOrEmpty(customerDTOs) ? new CustomerDTO(0L)
					: customerDTOs.get(0);
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : select.custumer ### {}", e.getMessage());
		}
		// in case of ana error => returning an empty object of customer with id extern = 0
		return new CustomerDTO(0L);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findByLoan(java.lang.Long)
	 */
	@Override
	public CustomerDTO findByLoan(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.custumer.loan");
			logger.info("idLoan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_LOAN_CONSTANT, idLoan);
			// execute query
			List<CustomerDTO> customerDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new CustomerLoanDTOAbacusRowMapper());

			return ACMValidationUtils.isNullOrEmpty(customerDTOs) ? new CustomerDTO(0L)
					: customerDTOs.get(0);
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : select.custumer.loan### {}", e.getMessage());
		}
		return new CustomerDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findAccountByLoan(java.lang.Long)
	 */
	@Override
	public List<CustomerAccountDTO> findAccountByLoan(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.custumer.account");
			logger.info("findAccountByLoan : idLoan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_LOAN_CONSTANT, idLoan);
			// execute query
			List<CustomerAccountDTO> customerAccountDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new CustomerAccountDTOAbacusRowMapper());
			// setting list Customer Account Schedule
			customerAccountDTOs.forEach(customerAccountDTO -> customerAccountDTO.setScheduleDTOs(
					findCustomerAccountScheduleByLoan(customerAccountDTO.getLoanId())));
			return customerAccountDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : select.custumer.account ### {}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findCustomerAccountScheduleByLoan(java.lang.Long)
	 */
	@Override
	public List<ScheduleDTO> findCustomerAccountScheduleByLoan(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.custumer.account.Schedule");
			logger.info("findCustomerAccountScheduleByLoan : idLoan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_LOAN_CONSTANT, idLoan);
			// execute query
			List<ScheduleDTO> scheduleDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new ScheduleAccountCustomerAbacusRowMapper());

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
			logger.error("### FAILED QUERY : select.custumer.account.Schedule ### {}",
					e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findAccountByCustomer(java.lang.Long)
	 */
	@Override
	public List<CustomerAccountDTO> findAccountByCustomer(Long idCustomer) {

		try {
			// load query
			String query = environment.getProperty("query.select.custumer.account.by.customer");
			logger.info("findAccountByCustomer : idCustomer= {}", idCustomer);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);
			// execute query
			List<CustomerAccountDTO> customerAccountDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new CustomerAccountDTOAbacusRowMapper());
			// setting list Customer Account Schedule
			customerAccountDTOs.forEach(customerAccountDTO -> customerAccountDTO.setScheduleDTOs(
					findCustomerAccountScheduleByLoan(customerAccountDTO.getLoanId())));
			return customerAccountDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : select.custumer.account.by.customer ### {}",
					e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findAllActiveAccountsForCustomer(java.lang.Long)
	 */
	@Override
	public List<CustomerActiveAccountDTO> findAllActiveAccountsForCustomer(Long idCustomer) {

		try {
			// load query
			String query =
					environment.getProperty("query.select.custumer.account.active.by.customer");
			logger.info("findAccountByCustomer : idCustomer= {}", idCustomer);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new CustomerActiveAccountDTOAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error(
					"### FAILED QUERY : query.select.custumer.account.active.by.customer ### {}",
					e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findMembersGroupByCustomer(java.lang.Long)
	 */
	@Override
	public List<CustomerMemberDTO> findMembersGroupByCustomer(Long idCustomer) {

		try {
			// load query
			String query = environment.getProperty("query.select.custumer.members.group");
			logger.info("findMembersGroupByCustomer : idCustomer= {}", idCustomer);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new CustomerMembersGRPAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.custumer.members.group ### {}",
					e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findMembersOrganisationByCustomer(java.lang.Long)
	 */
	@Override
	public List<CustomerMemberDTO> findMembersOrganisationByCustomer(Long idCustomer) {

		try {
			// load query
			String query = environment.getProperty("query.select.custumer.members.organisation");
			logger.info("findMembersOrganisationByCustomer : idCustomer= {}", idCustomer);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new CustomerMembersORGAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.custumer.members.organisation ### {}",
					e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findRelationshipByCustomer(java.lang.Long)
	 */
	@Override
	public List<CustomerMemberDTO> findRelationshipByCustomer(Long idCustomer) {

		try {
			// load query
			String query = environment.getProperty("query.select.custumer.relationship");
			logger.info("findRelationshipByCustomer : idCustomer= {}", idCustomer);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters,
					new CustomerRelationshipAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.custumer.relationship ### {}",
					e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findCustomerActiveAccount(java.lang.Long,
	 * java.lang.Long)
	 */
	@Override
	public Long findCustomerActiveAccount(Long idCustomer, Long idProduct) {

		try {
			// load query
			String query = environment.getProperty("query.select.active.account.customer");
			logger.info("findCustomerActiveAccount : idCustomer= {}", idCustomer);
			logger.info("findCustomerActiveAccount : idProduct= {}", idProduct);

			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer)
							.addValue(ID_PRODUCT_CONSTANT, idProduct);
			// execute query
			return namedParameterJdbcTemplate.queryForObject(query, namedParameters,
					new CustomerActiveAccountAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.custumer.relationship ### {}",
					e.getMessage());
		}
		return 0L;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#find()
	 */
	@Override
	public List<CustomerDTO> find() {

		try {
			// load query
			String query = environment.getProperty("query.select.custumer.all");
			// execute query
			return namedParameterJdbcTemplate.query(query, new CustomerDTOAbacusRowMapper());
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : select.custumer.all ### {}", e.getMessage());
		}
		// in case of ana error => returning an empty object of customer with id extern = 0
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findByLoan(java.lang.Long)
	 */
	@Override
	public ArrearsDTO findArrearsByCustomer(Long idCustomer) {

		try {
			// load query
			String query = environment.getProperty("query.select.customer.arrears");
			logger.info("idCustomer= {}", idCustomer);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);
			// execute query
			List<ArrearsDTO> arrearsDTOs = namedParameterJdbcTemplate.query(query, namedParameters,
					new ArrearsDTOAbacusRowMapper());

			return ACMValidationUtils.isNullOrEmpty(arrearsDTOs) ? new ArrearsDTO(0L)
					: arrearsDTOs.get(0);
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : select.customer.arrears### {}", e.getMessage());
		}
		return new ArrearsDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findCustomerPaidAccount(java.lang.Long,
	 * java.lang.Long)
	 */
	@Override
	public Double findCustomerPaidAccount(Long idCustomer, Long idProduct) {

		try {
			// load query
			String query = environment.getProperty("query.select.paid.account.customer");
			logger.info("findCustomerPaidAmount : idCustomer= {}", idCustomer);
			logger.info("findCustomerPaidAmount : idProduct= {}", idProduct);

			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer)
							.addValue(ID_PRODUCT_CONSTANT, idProduct);
			// execute query
			List<Double> paidAmount = namedParameterJdbcTemplate.query(query, namedParameters,
					new RowMapper<Double>() {
						@Override
						public Double mapRow(ResultSet rs, int rowNum) throws SQLException {

							return rs.getDouble("LAST_PAID_AMOUNT");
						}
					});

			if (!ACMValidationUtils.isNullOrEmpty(paidAmount)) {
				return paidAmount.get(0);
			}
			else {
				return 0D;
			}
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.paid.account.customer ### {}",
					e.getMessage());
		}
		return 0D;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#loadCheckPaiment(java.lang.String)
	 */
	@Override
	public Map<Long, List<ScheduleDTO>> loadCheckPaiment(String customerNumber) {

		try {
			logger.info("loadCheckPaiment : customerNumber= {}", customerNumber);
			// load query
			String query = environment.getProperty("query.aml.check.paiyment.customer");
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("customer_number", customerNumber);
			// execute query
			List<ScheduleDTO> scheduleDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new RowMapper<ScheduleDTO>() {
						@Override
						public ScheduleDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

							return new ScheduleDTO(rs.getInt("Period"), rs.getDate("RepaymentDate"),
									rs.getString("Customer_Name"), rs.getLong("CULoanID"),
									rs.getString("Customer_Number"), rs.getString("Paid_On"),
									rs.getBigDecimal("balance"), rs.getInt("TermPeriodNum"));
						}
					});
			// GROUP by CULoanID
			Map<Long, List<ScheduleDTO>> groupByCULoanIDMap = scheduleDTOs.stream()
					.collect(Collectors.groupingBy(ScheduleDTO::getIdLoanExtern));
			logger.info("groupByCULoanIDMap = {}", groupByCULoanIDMap);
			logger.info("AML check : load paiment data for given customer from ABACUS DB :: DONE");
			// returning result
			return groupByCULoanIDMap;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.aml.check.paiyment.customer ### {}",
					e.getMessage());
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findCustomerPaidAccounts(java.lang.Long)
	 */
	@Override
	public List<Double> findCustomerPaidAccounts(Long idCustomer) {

		try {
			// load query
			String query = environment.getProperty("query.select.paid.accounts.customer");
			logger.info("findCustomerPaidAmount : idCustomer= {}", idCustomer);

			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomer);
			// execute query
			List<Double> lastPaidAmounts = namedParameterJdbcTemplate.query(query, namedParameters,
					new RowMapper<Double>() {
						@Override
						public Double mapRow(ResultSet rs, int rowNum) throws SQLException {

							return rs.getDouble("LAST_PAID_AMOUNT");
						}
					});

			if (!ACMValidationUtils.isNullOrEmpty(lastPaidAmounts)) {
				return lastPaidAmounts;
			}
			else {
				return new ArrayList<>();
			}
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.paid.account.customer ### {}",
					e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findCustomers(java.lang.Long)
	 */
	@Override
	public List<CustomerDTO> findCustomers(Long idCustomerExtern) {

		try {
			// load query
			String query = environment.getProperty("query.select.customers.for.batch");
			logger.info("find by idCustomer= {}", idCustomerExtern);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_CUSTOMER_CONSTANT, idCustomerExtern);
			// execute query
			List<CustomerDTO> customerDTOs = namedParameterJdbcTemplate.query(query,
					namedParameters, new CustomerDTOAbacusRowMapper());

			return ACMValidationUtils.isNullOrEmpty(customerDTOs) ? new ArrayList<>()
					: customerDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : select.custumer ### {}", e.getMessage());
		}
		// in case of ana error => returning an empty object of customer with id extern = 0
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerAbacusService#findLoanDetailsInformationsByLoan(java.lang.Long)
	 */
	@Override
	public List<LoanDetailsInfoResponseDTO> findLoanDetailsInformationsByLoan(Long idLoan) {

		try {
			// load query
			String query = environment.getProperty("query.select.loan.details.informations");
			logger.info("INIT query.select.loan.details.informations : idLoan= {}", idLoan);
			// init params
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue(ID_LOAN_CONSTANT, idLoan);
			// execute query
			List<LoanDetailsInfoResponseDTO> listLoanDetailsInformationsDTOs =
					namedParameterJdbcTemplate.query(query, namedParameters,
							new LoanDetailsInformationsRowMapper());

			logger.info("### list Loan Details InformationsDTOs = [{}] ### ",
					listLoanDetailsInformationsDTOs.toString());

			return ACMValidationUtils.isNullOrEmpty(listLoanDetailsInformationsDTOs)
					? new ArrayList<>()
					: listLoanDetailsInformationsDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("### FAILED QUERY : query.select.loan.details.informations ### {}",
					e.getMessage());
		}
		return new ArrayList<>();

	}
}
