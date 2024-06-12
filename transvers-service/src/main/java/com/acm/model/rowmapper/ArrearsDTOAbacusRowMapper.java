/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.ArrearsDTO;

/**
 * {@link ArrearsDTOAbacusRowMapper} class.
 * 
 * @author Salmen Fatnassi
 * @since 1.1.3
 */
public class ArrearsDTOAbacusRowMapper implements RowMapper<ArrearsDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public ArrearsDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		ArrearsDTO arrearsDTO = new ArrearsDTO();
		arrearsDTO.setCustomerIdExtern(rs.getLong("CUSTOMERID"));
		arrearsDTO.setArrearDay(rs.getLong("ARREARDAY"));
		arrearsDTO.setArrearSchedule(rs.getLong("ARREARSCHEDULE"));
		return arrearsDTO;
	}

}
