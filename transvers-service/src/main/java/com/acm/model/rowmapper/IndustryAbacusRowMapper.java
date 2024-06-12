/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.IndustryDTO;

/**
 * {@link IndustryAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class IndustryAbacusRowMapper implements RowMapper<IndustryDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public IndustryDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new IndustryDTO(rs.getLong("IndustryID"), rs.getString("Name"),
				rs.getInt("DefaultItem"), rs.getBoolean("Active"));
	}
}
