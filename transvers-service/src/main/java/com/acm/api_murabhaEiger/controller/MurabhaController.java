/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_murabhaEiger.controller;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_murabhaEiger.service.MurabhaService;
import com.acm.utils.dtos.ConfirmPurchaseOrSaleRequestApiDTO;
import com.acm.utils.dtos.ConfirmPurchaseResponseApiDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiRequestDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiResponseDTO;
import com.acm.utils.dtos.SaleMurabhaApiRequestDTO;
import com.fasterxml.jackson.core.JsonProcessingException;

/**
 * The Class MurabhaController.
 */
@RestController
@RequestMapping("/murabha-api")
public class MurabhaController {

	/** The murabha service. */
	@Autowired
	private MurabhaService murabhaService;

	/**
	 * Gets the token.
	 *
	 * @return the token
	 * @throws JsonProcessingException the json processing exception
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	@PostMapping("/get-token")
	public ResponseEntity<String> getToken() throws JsonProcessingException, InvalidKeyException,
			NoSuchAlgorithmException, IllegalArgumentException, IllegalAccessException {

		return murabhaService.getToken();

	}

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
	@PostMapping("/purchase")
	public ResponseEntity<PurchaseMurabhaApiResponseDTO> purchase(
			@RequestBody PurchaseMurabhaApiRequestDTO purchaseRequestDTO,
			@RequestParam(value = "loanId") Long loanId) throws InvalidKeyException,
			NoSuchAlgorithmException, IllegalArgumentException, IllegalAccessException {

		return murabhaService.purchase(purchaseRequestDTO, loanId);

	}

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
	@PostMapping("/confirm-purchase")
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> confirmPurchase(
			@RequestBody ConfirmPurchaseOrSaleRequestApiDTO confirmPurchaseRequestDTO,
			@RequestParam(value = "loanId") Long loanId) throws InvalidKeyException,
			NoSuchAlgorithmException, IllegalArgumentException, IllegalAccessException {

		return murabhaService.confirmPurchase(confirmPurchaseRequestDTO, loanId);

	}

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
	@PostMapping("/sale")
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> sale(
			@RequestBody SaleMurabhaApiRequestDTO salePurchaseRequestDTO,
			@RequestParam(value = "loanId") Long loanId) throws InvalidKeyException,
			NoSuchAlgorithmException, IllegalArgumentException, IllegalAccessException {

		return murabhaService.sale(salePurchaseRequestDTO, loanId);

	}

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
	@PostMapping("/confirm-sale")
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> confirmSale(
			@RequestBody ConfirmPurchaseOrSaleRequestApiDTO confirmSaleRequestDTO,
			@RequestParam(value = "loanId") Long loanId) throws InvalidKeyException,
			NoSuchAlgorithmException, IllegalArgumentException, IllegalAccessException {

		return murabhaService.confirmSale(confirmSaleRequestDTO, loanId);

	}

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
	@PostMapping("/cancel-transaction")
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> cancelTransaction(
			@RequestBody ConfirmPurchaseOrSaleRequestApiDTO cancelTransaction)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException {

		return murabhaService.cancelTransaction(cancelTransaction);

	}

	/**
	 * Transfer notice.
	 *
	 * @param transferNotices the transfer notices
	 * @param loanId the loan id
	 * @return the response entity
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	@PostMapping("/transfer-notice")
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> transferNotice(
			@RequestBody ConfirmPurchaseOrSaleRequestApiDTO transferNotices,
			@RequestParam(value = "loanId") Long loanId) throws InvalidKeyException,
			NoSuchAlgorithmException, IllegalArgumentException, IllegalAccessException {

		return murabhaService.transferNotice(transferNotices, loanId);

	}

}
