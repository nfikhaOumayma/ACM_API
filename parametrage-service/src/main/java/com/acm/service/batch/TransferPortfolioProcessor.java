package com.acm.service.batch;

import org.springframework.batch.item.ItemProcessor;

import com.acm.utils.dtos.CUAccountPortfolioTransferredDTO;

public class TransferPortfolioProcessor implements
		ItemProcessor<CUAccountPortfolioTransferredDTO, CUAccountPortfolioTransferredDTO> {

	@Override
	public CUAccountPortfolioTransferredDTO process(CUAccountPortfolioTransferredDTO item)
			throws Exception {

		// TODO Auto-generated method stub

		System.out.print(item);
		return null;
	}

}
