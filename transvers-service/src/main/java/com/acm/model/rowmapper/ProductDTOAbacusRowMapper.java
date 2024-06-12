/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.ProductDTO;

/**
 * {@link ProductDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
public class ProductDTOAbacusRowMapper implements RowMapper<ProductDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public ProductDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new ProductDTO(rs.getString("Code"), rs.getString("Description"),
				rs.getLong("ProductID"), rs.getLong("ProductTypeID"), rs.getDate("CreationDate"),
				rs.getDate("EditDate"), rs.getBoolean("Active"), rs.getDate("RateStartDate"),
				rs.getDate("RateEndDate"), rs.getBigDecimal("Rate"),
				rs.getBigDecimal("MAXIMUMBALANCE"), rs.getInt("MINIMUMTERM"),
				rs.getInt("MAXIMUMTERM"), rs.getBigDecimal("IssueFeepercentage1"),
				rs.getBigDecimal("IssueFeepercentage2"), rs.getBigDecimal("IssueFeepercentage3"),
				rs.getBigDecimal("IssueFeepercentage4"), rs.getBoolean("UseScheduleInterest"),
				rs.getBoolean("CapitaliseInterestWhenRefinancing"), rs.getInt("DecimalPlaces"),
				rs.getString("CURRENCY"), rs.getInt("MAXIMUMMEMAGE"), rs.getInt("MINIMUMMEMAGE"),
				rs.getInt("MAXACCOUNTS"), rs.getInt("MAXIMUMDEFERREDPERIOD"),
				rs.getInt("MINIMUMDEFERREDPERIOD"), rs.getInt("cuInsuranceID"),
				rs.getBigDecimal("IssueFeeVAT1"), rs.getBigDecimal("IssueFeeVAT2"),
				rs.getString("CustomerType"), rs.getBigDecimal("InsuranceVat"),
				rs.getString("ROUND_TYPE"), rs.getBigDecimal("ISSUEFEEAMOUNT1"),
				rs.getBigDecimal("ISSUEFEEAMOUNT2"), rs.getBigDecimal("FLAT_INTEREST_RATE"));
	}
}
