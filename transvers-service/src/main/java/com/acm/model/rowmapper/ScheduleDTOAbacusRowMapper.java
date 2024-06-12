/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.ScheduleDTO;

/**
 * {@link ScheduleDTOAbacusRowMapper} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public class ScheduleDTOAbacusRowMapper implements RowMapper<ScheduleDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public ScheduleDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		ScheduleDTO scheduleDTO = new ScheduleDTO();
		scheduleDTO.setPeriod(rs.getInt("PERIOD"));
		scheduleDTO.setRepaymentDate(rs.getDate("REPAYMENT_DATE"));
		scheduleDTO.setTotalRepayment(rs.getBigDecimal("TOTAL_REPAYMENT"));
		scheduleDTO.setLoanRepayment(rs.getBigDecimal("LOAN_REPAYMENT"));
		scheduleDTO.setInterestRepayment(rs.getBigDecimal("INTEREST_REPAYMENT"));
		scheduleDTO.setBalance(rs.getBigDecimal("BALANCE"));
		return scheduleDTO;
	}

}
