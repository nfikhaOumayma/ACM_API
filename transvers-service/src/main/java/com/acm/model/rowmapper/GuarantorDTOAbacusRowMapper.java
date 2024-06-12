/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.GuarantorDTO;

/**
 * {@link GuarantorDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
public class GuarantorDTOAbacusRowMapper implements RowMapper<GuarantorDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public GuarantorDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new GuarantorDTO(rs.getString("Name"), rs.getDate("RegistrationDate"),
				rs.getString("Branch"), rs.getInt("Age"), rs.getString("Address1"),
				rs.getString("Address2"), rs.getString("Address3"), rs.getString("TownCity"),
				rs.getString("County"), rs.getString("State"), rs.getString("PostalCode"),
				rs.getString("Country"), rs.getString("GuarantorType"), rs.getString("Currency"),
				rs.getBigDecimal("OutstandingBalance"), rs.getBigDecimal("Amount"),
				rs.getLong("CULoanID"), rs.getLong("CustomerID"));
	}
}
