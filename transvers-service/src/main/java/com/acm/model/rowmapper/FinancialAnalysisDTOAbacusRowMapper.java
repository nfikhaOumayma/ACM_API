/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.FinancialAnalysisDTO;

/**
 * {@link FinancialAnalysisDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public class FinancialAnalysisDTOAbacusRowMapper implements RowMapper<FinancialAnalysisDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public FinancialAnalysisDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new FinancialAnalysisDTO(rs.getBigDecimal("CurrentAssets"),
				rs.getBigDecimal("FixedAssets"), rs.getBigDecimal("Assets"),
				rs.getBigDecimal("CurrentLiabilities"), rs.getBigDecimal("FixedLiabilities"),
				rs.getBigDecimal("TotalLiabilities"), rs.getBigDecimal("Equity"),
				rs.getBigDecimal("MonthlySales"), rs.getBigDecimal("CotsOFGoodSold"),
				rs.getBigDecimal("GrossProfit"), rs.getBigDecimal("MonthlyExpense"),
				rs.getBigDecimal("NetProfit"), rs.getBigDecimal("OtherIncome"),
				rs.getBigDecimal("FamilyExpense"), rs.getBigDecimal("HouseholdSurplus"),
				rs.getBigDecimal("ExcludedAssets"), rs.getBigDecimal("CollateralAmount"),
				rs.getBigDecimal("ApplyAmountTotal"), rs.getDate("MaturityDate"),
				rs.getBigDecimal("LoanTerm"), rs.getBigDecimal("Installement"),
				rs.getBigDecimal("WorkingCapital"), rs.getBigDecimal("Grossmargin"),
				rs.getBigDecimal("Debt_Equity"), rs.getBigDecimal("Inventory_Turnover"),
				rs.getBigDecimal("Installment_Coverage"), rs.getBigDecimal("Loan_Recommendation"),
				rs.getBigDecimal("Guarantor_Coverage"));
	}
}
