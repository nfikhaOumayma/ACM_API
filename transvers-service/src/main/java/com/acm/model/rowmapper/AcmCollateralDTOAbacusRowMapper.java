/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.AcmCollateralDTO;

/**
 * {@link AcmCollateralDTOAbacusRowMapper} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class AcmCollateralDTOAbacusRowMapper implements RowMapper<AcmCollateralDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public AcmCollateralDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new AcmCollateralDTO(rs.getLong("CULoanID"), rs.getString("Reference"),
				rs.getString("Description"), rs.getString("CollateralType"),
				rs.getBigDecimal("OriginalGrossValue"), rs.getBigDecimal("GrossValue"),
				rs.getBigDecimal("RealisedValue"), rs.getBigDecimal("FixedCost"),
				rs.getBigDecimal("NetValue"), rs.getString("FullName"));
	}

}
