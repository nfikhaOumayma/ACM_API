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
 * {@link ScheduleAccountCustomerAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
public class ScheduleAccountCustomerAbacusRowMapper implements RowMapper<ScheduleDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public ScheduleDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		ScheduleDTO scheduleDTO = new ScheduleDTO();
		scheduleDTO.setPeriod(rs.getInt("Period"));
		scheduleDTO.setRepaymentDate(rs.getDate("RepaymentDate"));
		scheduleDTO.setTotalRepayment(rs.getBigDecimal("TotalRepayment"));
		scheduleDTO.setLoanRepayment(rs.getBigDecimal("LoanRepayment"));
		scheduleDTO.setInterestRepayment(rs.getBigDecimal("InterestRepayment"));
		scheduleDTO.setBalance(rs.getBigDecimal("Balance"));
		scheduleDTO.setRepaidOn(rs.getDate("RepaidOn"));
		scheduleDTO.setInterestAmtPaid(rs.getBigDecimal("InterestAmtPaid").toString());
		scheduleDTO.setLoanRepaymentPaid(rs.getBigDecimal("LoanRepaymentPaid").toString());
		scheduleDTO.setNbArrearsDays(rs.getBigDecimal("NBArrearsDays"));
		scheduleDTO.setStatus(rs.getBigDecimal("Status"));
		return scheduleDTO;
	}
}
