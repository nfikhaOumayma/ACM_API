/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.BrancheDTO;

/**
 * {@link BrancheDTOAbacusRowMapper} class.
 *
 * @author YesserSomai
 * @since 1.0.2
 */
public class BrancheDTOAbacusRowMapper implements RowMapper<BrancheDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public BrancheDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new BrancheDTO(rs.getLong("BRANCHID"), rs.getString("NAME"),
				rs.getString("DESCRIPTION"), rs.getString("Code"), rs.getString("SortCode"),
				rs.getBoolean("AccessAll"), rs.getBoolean("AllowCustomers"),
				rs.getBoolean("BackOfficeGL"), rs.getBoolean("Active"), rs.getLong("AddressID"),
				rs.getString("BranchPhoneNumber"), rs.getString("BranchFaxNumber"),
				rs.getString("BranchEmail"), rs.getInt("IncrementDays"),
				rs.getInt("UseClosedDaysHolidays"), rs.getInt("ClosedDays"),
				rs.getInt("ChequeClearancePersonal"), rs.getInt("ChequeClearanceThirdParty"),
				rs.getLong("ParentBranchID"), rs.getLong("RegionalManagerID"),
				rs.getString("GLAccountCode"));
	}
}
