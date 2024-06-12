/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.number.NumberUtils;

/**
 * {@link LoanDTOAbacusRowMapper} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public class LoanDTOAbacusRowMapper implements RowMapper<LoanDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public LoanDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		LoanDTO loanDTO = new LoanDTO();
		loanDTO.setIdLoanExtern(rs.getLong("CULoanID"));
		loanDTO.setPortfolioId(rs.getLong("CUAccountPortfolioID"));
		loanDTO.setAccountNumber(rs.getString("AccountNumber"));
		loanDTO.setIdAccountExtern(rs.getLong("CUAccountID"));
		loanDTO.setProductDescription(rs.getString("LOANPRODUCT_DESCRIPTION"));
		loanDTO.setProductCode(rs.getString("LOANPRODUCT_CODE"));
		loanDTO.setCustomerName(rs.getString("CUSTOMER_NAME"));
		loanDTO.setCurrencySymbol(rs.getString("CURRENCY_SYMBOL"));
		loanDTO.setCurrencyDecimalPlaces(rs.getInt("CURRENCY_DECIMALPLACES"));
		loanDTO.setApplyDate(rs.getDate("APPLYDATE"));
		loanDTO.setPortfolioCode(rs.getString("ACCOUNTPORTFOLIO_CODE"));
		loanDTO.setPortfolioDescription(rs.getString("ACCOUNTPORTFOLIO_DESCRIPTION"));
		loanDTO.setProductId(rs.getInt("ProductID"));
		loanDTO.setCustomerId(rs.getLong("CUSTOMERID"));

		loanDTO.setStatut(rs.getInt("STATUS"));
		loanDTO.setApplyAmountTotal(rs.getBigDecimal("APPLY_AMOUNT_TOTAL"));
		loanDTO.setCreationDate(rs.getDate("CREATION_DATE"));
		loanDTO.setTermPeriodNum(rs.getInt("TERM_PERIOD"));
		loanDTO.setPaymentFreq(rs.getInt("PERIOD_FREQ"));
		loanDTO.setIssueDate(rs.getDate("ISSUE_DATE"));
		loanDTO.setIssueFeeAmount(rs.getBigDecimal("ISSUE_FEE_AMOUNT"));
		loanDTO.setProductRate(NumberUtils.roundBigDecimal(rs.getBigDecimal("PRODUCT_RATE"), 2,
				BigDecimal.ROUND_HALF_EVEN));
		loanDTO.setGracePeriod(rs.getInt("GRACEPERIOD"));
		loanDTO.setIndustryCode(rs.getString("ACCOUNTINDUSTRYCODE_CODE"));
		loanDTO.setIndustryCodeDescription(rs.getString("ACCOUNTINDUSTRYCODE_DESCRIPTION"));
		loanDTO.setLoanReasonCode(rs.getString("PRODUCTLOANREASONS_CODE"));
		loanDTO.setLoanReasonDescription(rs.getString("PRODUCTLOANREASONS_DESCRIPTION"));

		loanDTO.setInitialPaymentDate(rs.getDate("InitialPaymentDate"));
		loanDTO.setNormalPayment(rs.getLong("NormalPayment"));
		loanDTO.setIgnoreOddDays(rs.getBoolean("IgnoreOddDays"));
		loanDTO.setPeriodsDeferred(rs.getInt("PeriodsDeferred"));
		loanDTO.setCalculateInitialPaymentDate(rs.getBoolean("CalculateInitialPaymentDate"));
		loanDTO.setTermPeriodID(rs.getLong("TermPeriodID"));

		loanDTO.setBranchID(rs.getInt("BranchID"));
		loanDTO.setBranchName(rs.getString("BRANCHE_NAME"));
		loanDTO.setBranchDescription(rs.getString("BRANCHE_DESCRIPTION"));

		loanDTO.setCustomerType(CustomerType.typeName(rs.getInt("CUSTOMER_TYPE")));
		loanDTO.setCommunityCULoanID(rs.getLong("CommunityCULoanID"));

		loanDTO.setGuarantorSourceId(rs.getInt("GUARANTOR_SOURCE_ID"));
		loanDTO.setSourceOfFundsID(rs.getInt("SOURCE_OF_FUNDS_ID"));
		loanDTO.setRefinanceReasonId(rs.getInt("REFINANCE_REASON_ID"));
		loanDTO.setDistrictCodeId(rs.getInt("DISTRICT_CODE_ID"));
		loanDTO.setIntPayPeriodNum(rs.getInt("INT_PAY_PERIOD_NUM"));

		loanDTO.setLoanCalculationMode(rs.getInt("loanCalculationMode"));
		loanDTO.setApr(NumberUtils.roundBigDecimal(rs.getBigDecimal("APR"), 2,
				BigDecimal.ROUND_HALF_EVEN));
		loanDTO.setEffectiveIntRate(NumberUtils.roundBigDecimal(
				rs.getBigDecimal("EffectiveIntRate_EIR"), 2, BigDecimal.ROUND_HALF_EVEN));
		return loanDTO;
	}
}
