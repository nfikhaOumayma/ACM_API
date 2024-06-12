/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_murabhaEiger.service;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import org.springframework.http.ResponseEntity;

import com.acm.utils.dtos.ConfirmPurchaseOrSaleRequestApiDTO;
import com.acm.utils.dtos.ConfirmPurchaseResponseApiDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiRequestDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiResponseDTO;
import com.acm.utils.dtos.SaleMurabhaApiRequestDTO;

/**
 * The Interface MurabhaService.
 */
public interface MurabhaService {

	/**
	 * Gets the token.
	 *
	 * @return the token
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	ResponseEntity<String> getToken() throws InvalidKeyException, NoSuchAlgorithmException,
			IllegalArgumentException, IllegalAccessException;

	/**
	 * Purchase.
	 *
	 * @param purchaseRequestDTO the purchase request DTO
	 * @param loanId the loan id
	 * @return the response entity
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	ResponseEntity<PurchaseMurabhaApiResponseDTO> purchase(
			PurchaseMurabhaApiRequestDTO purchaseRequestDTO, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException;

	/**
	 * Confirm purchase.
	 *
	 * @param confirmPurchaseRequestDTO the confirm purchase request DTO
	 * @param loanId the loan id
	 * @return the response entity
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	ResponseEntity<ConfirmPurchaseResponseApiDTO> confirmPurchase(
			ConfirmPurchaseOrSaleRequestApiDTO confirmPurchaseRequestDTO, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException;

	/**
	 * Sale.
	 *
	 * @param salePurchaseRequestDTO the sale purchase request DTO
	 * @param loanId the loan id
	 * @return the response entity
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	ResponseEntity<ConfirmPurchaseResponseApiDTO> sale(
			SaleMurabhaApiRequestDTO salePurchaseRequestDTO, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException;

	/**
	 * Confirm sale.
	 *
	 * @param confirmSaleRequestDTO the confirm sale request DTO
	 * @param loanId the loan id
	 * @return the response entity
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	ResponseEntity<ConfirmPurchaseResponseApiDTO> confirmSale(
			ConfirmPurchaseOrSaleRequestApiDTO confirmSaleRequestDTO, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException;

	/**
	 * Cancel transaction.
	 *
	 * @param cancelTransaction the cancel transaction
	 * @return the response entity
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	ResponseEntity<ConfirmPurchaseResponseApiDTO> cancelTransaction(
			ConfirmPurchaseOrSaleRequestApiDTO cancelTransaction) throws InvalidKeyException,
			NoSuchAlgorithmException, IllegalArgumentException, IllegalAccessException;

	/**
	 * Transfer notice.
	 *
	 * @param transferNotice the transfer notice
	 * @param loanId the loan id
	 * @return the response entity
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	ResponseEntity<ConfirmPurchaseResponseApiDTO> transferNotice(
			ConfirmPurchaseOrSaleRequestApiDTO transferNotice, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException;

}
