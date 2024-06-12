/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.ApplicationFeeDTO;

/**
 * {@link ApplicationFeesDTOAbacusRowMapper} class.
 *
 * @author Salmen Fatnassi
 * @since 1.0.8
 */
public class ApplicationFeesDTOAbacusRowMapper implements RowMapper<ApplicationFeeDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public ApplicationFeeDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		ApplicationFeeDTO applicationFeeDTO = new ApplicationFeeDTO();
		applicationFeeDTO.setCufeeID(rs.getLong("CUFeeID"));
		applicationFeeDTO.setCode(rs.getString("Code"));
		applicationFeeDTO.setDescription(rs.getString("Description"));
		applicationFeeDTO.setProductId(rs.getInt("ProductID"));

		return applicationFeeDTO;
	}
}
