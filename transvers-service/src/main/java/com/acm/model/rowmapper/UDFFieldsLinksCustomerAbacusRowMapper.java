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
 * {@link UDFFieldsLinksCustomerAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class UDFFieldsLinksCustomerAbacusRowMapper implements RowMapper<UserDefinedFieldsLinksDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public UserDefinedFieldsLinksDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		UserDefinedFieldsDTO userDefinedFieldsDTO = new UserDefinedFieldsDTO();
		userDefinedFieldsDTO.setIdUDFField(rs.getLong("UserDefinedFieldID"));
		UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO = new UserDefinedFieldsLinksDTO();
		userDefinedFieldsLinksDTO.setUserDefinedFieldsDTO(userDefinedFieldsDTO);
		userDefinedFieldsLinksDTO.setIdAbacusUDFLink(rs.getLong("UserDefinedFieldLinkID"));
		userDefinedFieldsLinksDTO.setFieldValue(rs.getString("Value"));
		userDefinedFieldsLinksDTO.setCustomerId(rs.getLong("CustomerID"));
		userDefinedFieldsLinksDTO.setUdfListValueId(rs.getLong("UserDefinedFieldListID"));
		userDefinedFieldsLinksDTO.setSurveysId(rs.getLong("SurveyID"));

		return userDefinedFieldsLinksDTO;
	}
}
