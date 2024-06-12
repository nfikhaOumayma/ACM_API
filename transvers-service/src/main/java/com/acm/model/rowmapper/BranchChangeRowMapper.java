/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.BranchChangeDTO;

/**
 * {@link BranchChangeRowMapper} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class BranchChangeRowMapper implements RowMapper<BranchChangeDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public BranchChangeDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		BranchChangeDTO branchChangeDTO = new BranchChangeDTO();
		branchChangeDTO.setId(rs.getLong("BranchChangeID"));
		branchChangeDTO.setCustomerId(rs.getLong("CustomerID"));
		branchChangeDTO.setBranchId(rs.getInt("BranchID"));
		branchChangeDTO.setStartDate(rs.getDate("startDate"));
		branchChangeDTO.setBranchDescription(rs.getString("BranchDescription"));
		branchChangeDTO.setBranchName(rs.getString("BranchName"));
		return branchChangeDTO;
	}

}
