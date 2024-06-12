/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.LoanSourceOfFundsDTO;

/**
 * {@link LoanSourceOfFundsAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanSourceOfFundsAbacusRowMapper implements RowMapper<LoanSourceOfFundsDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public LoanSourceOfFundsDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new LoanSourceOfFundsDTO(rs.getLong("LoanSourceOfFundsID"), rs.getString("Code"),
				rs.getString("Description"), rs.getBoolean("AllProducts"),
				rs.getString("LoanProductIDs"), rs.getInt("DefaultItem"), rs.getBoolean("Active"),
				rs.getLong("HighLevelSourceFund"), rs.getLong("sourceofFundtype"),
				rs.getLong("CurrencyID"));
	}
}
