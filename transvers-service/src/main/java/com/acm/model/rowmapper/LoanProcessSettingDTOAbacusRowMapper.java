/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.LoanProcessSettingDTO;

/**
 * {@link LoanProcessSettingSettingDTOAbacusRowMapper} class.
 *
 * @author RadhouaneHomrani
 * @since 0.2.0
 */
public class LoanProcessSettingDTOAbacusRowMapper implements RowMapper<LoanProcessSettingDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public LoanProcessSettingDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		LoanProcessSettingDTO loanProcessDTO = new LoanProcessSettingDTO();
		loanProcessDTO.setProductId(rs.getLong("PRODUCT_ID"));
		loanProcessDTO.setProductCode(rs.getString("PRODUCT_CDOE"));
		loanProcessDTO.setProductProcessCode(rs.getString("PROCESS_CODE"));
		loanProcessDTO.setProductProcessDescription(rs.getString("PROCESS_DESCRIPTION"));
		loanProcessDTO.setProductProcessRequire(rs.getBoolean("PROCESS_REQUIRED"));
		loanProcessDTO.setProductLoanProcessesID(rs.getInt("CUProductLoanProcessesID"));
		loanProcessDTO.setMenuKey(rs.getString("MenuKey"));
		loanProcessDTO.setApply(rs.getBoolean("Apply"));
		loanProcessDTO.setApprove(rs.getBoolean("Approve"));
		return loanProcessDTO;
	}
}
