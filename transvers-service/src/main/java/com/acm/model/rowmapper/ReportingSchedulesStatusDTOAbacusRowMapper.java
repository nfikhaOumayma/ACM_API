/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.ReportingSchedulesStatusDTO;

/**
 * {@link ReportingSchedulesStatusDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.1.2
 */
public class ReportingSchedulesStatusDTOAbacusRowMapper
		implements RowMapper<ReportingSchedulesStatusDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public ReportingSchedulesStatusDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new ReportingSchedulesStatusDTO(rs.getString("ACCOUNTNUMBER"),
				rs.getString("GROUPNUMBER"), rs.getString("GROUPNAME"),
				rs.getString("CUSTOMERNUMBER"), rs.getString("CUSTOMERNAME"),
				rs.getInt("CUSTOMERTYPE"), rs.getString("CUSTOMERTYPELABEL"),
				rs.getInt("CUSTOMERGENDER"), rs.getString("CUSTOMERGENDERLABEL"),
				rs.getDate("REPAYMENTDATE"), rs.getLong("INSTALMENTAMOUNT"),
				rs.getLong("INSTALMENTPRINCIPAL"), rs.getLong("INSTALMENTINTEREST"),
				rs.getLong("INSTALMENTPRINCIPALPAID"), rs.getLong("INSTALMENTINTERESTPAID"),
				rs.getLong("INSTALMENTTOTALPAID"), rs.getLong("TOTALPRINCIPALPAID"),
				rs.getLong("TOTALINTERESTPAID"), rs.getLong("TOTALPAIDAMOUNT"),
				rs.getLong("UNPAIDPRINCIPAL"), rs.getLong("UPAIDINTEREST"),
				rs.getLong("UPAIDAMOUNT"), rs.getInt("LATEDAYS"), rs.getInt("NBUNPAIDINSTALMENT"),
				rs.getLong("REMAININGPRINCIPAL"), rs.getLong("REMAININGINTEREST"),
				rs.getLong("REMAININGAMOUNT"), rs.getInt("SOURCEOFFUNDS"),
				rs.getString("SOURCEOFFUNDSLABEL"), rs.getInt("BRANCHE"),
				rs.getString("BRANCHELABEL"), rs.getInt("LOANOFFICER"),
				rs.getString("LOANOFFICERLABEL"), rs.getInt("LOANREASONID"),
				rs.getString("LOANREASONIDLABEL"), rs.getInt("PRODUCTID"),
				rs.getString("PRODUCTIDLABEL"), rs.getInt("LOANSTATUS"),
				rs.getString("LOANSTATUSLABEL"), rs.getDate("ISSUEDATE"));
	}
}
