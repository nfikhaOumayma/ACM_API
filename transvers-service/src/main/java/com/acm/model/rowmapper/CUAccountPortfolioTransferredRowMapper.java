/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.CUAccountPortfolioTransferredDTO;

/**
 * {@link CUAccountPortfolioTransferredRowMapper} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class CUAccountPortfolioTransferredRowMapper
		implements RowMapper<CUAccountPortfolioTransferredDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public CUAccountPortfolioTransferredDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		CUAccountPortfolioTransferredDTO accountPortfolioTransferredDTO =
				new CUAccountPortfolioTransferredDTO();
		accountPortfolioTransferredDTO.setId(rs.getLong("CUAccountPortfolioTransferID"));
		accountPortfolioTransferredDTO.setToPorfolio(rs.getLong("ToPortfolioID"));
		accountPortfolioTransferredDTO.setFromPortfolio(rs.getLong("FromPortfolioID"));
		accountPortfolioTransferredDTO.setTransferedDate(rs.getDate("TransferedDate"));
		accountPortfolioTransferredDTO.setCustomerId(rs.getLong("CustomerID"));
		accountPortfolioTransferredDTO.setProductType(rs.getString("ProductType"));

		accountPortfolioTransferredDTO.setToPorfolioName(rs.getString("ToPortfolio"));
		accountPortfolioTransferredDTO.setToPorfolioCode(rs.getString("ToPortfolio"));

		accountPortfolioTransferredDTO.setCuAccountId(rs.getLong("CUAccountID"));

		return accountPortfolioTransferredDTO;
	}

}
