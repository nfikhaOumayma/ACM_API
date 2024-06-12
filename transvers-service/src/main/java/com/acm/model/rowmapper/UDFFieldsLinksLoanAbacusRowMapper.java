/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;

/**
 * {@link UDFFieldsLinksLoanAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class UDFFieldsLinksLoanAbacusRowMapper implements RowMapper<UserDefinedFieldsLinksDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public UserDefinedFieldsLinksDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		UserDefinedFieldsDTO userDefinedFieldsDTO = new UserDefinedFieldsDTO();
		userDefinedFieldsDTO.setIdUDFField(rs.getLong("UserDefinedFieldID"));
		UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO = new UserDefinedFieldsLinksDTO();
		userDefinedFieldsLinksDTO.setIdAbacusUDFLink(rs.getLong("UserDefinedFieldLinkID"));
		userDefinedFieldsLinksDTO.setUserDefinedFieldsDTO(userDefinedFieldsDTO);
		userDefinedFieldsLinksDTO.setFieldValue(rs.getString("Value"));
		userDefinedFieldsLinksDTO.setCuAccountId(rs.getLong("CUAccountID"));
		userDefinedFieldsLinksDTO.setUdfListValueId(rs.getLong("UserDefinedFieldListID"));
		userDefinedFieldsLinksDTO.setSurveysId(rs.getLong("SurveyID"));
		return userDefinedFieldsLinksDTO;
	}
}
