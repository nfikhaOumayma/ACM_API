/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.DeferredPeriodTypeDTO;

/**
 * {@link DeferredPeriodTypeAbacusRowMapper} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class DeferredPeriodTypeAbacusRowMapper implements RowMapper<DeferredPeriodTypeDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public DeferredPeriodTypeDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new DeferredPeriodTypeDTO(rs.getLong("ValueMember"), rs.getString("DisplayMember"));
	}

}
