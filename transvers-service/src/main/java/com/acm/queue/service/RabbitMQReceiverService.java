/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.queue.service;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.queue.service.impl.RabbitMQReceiverServiceImpl;
import com.acm.utils.dtos.ClaimNoteDTO;

/**
 * The RabbitMQReceiverService interface is responsible for handling messages received from RabbitMQ
 * queues. It defines methods for queuing messages and processing specific types of messages.
 * {@link RabbitMQReceiverServiceImpl} class provides an implementation of this interface.
 *
 * @author nrmila
 * @since 1.0.8
 */
public interface RabbitMQReceiverService {

	/**
	 * Received customer.
	 *
	 * @param idCustomerExternal the id customer external
	 */
	void receivedCustomer(String idCustomerExternal);

	/**
	 * Received loan.
	 *
	 * @param loanAccountNumber the loan account number
	 */
	void receivedLoan(String loanAccountNumber);

	/**
	 * Received validate loan.
	 *
	 * @param idIbLoan the id ib loan
	 */
	void receivedValidateLoan(String idIbLoan);

	/**
	 * Gets the loan schedules for IB.
	 *
	 * @param idIbCustomer the id ib customer
	 * @throws ApiAbacusException the api abacus exception
	 */
	void getLoanSchedulesForIB(String idIbCustomer) throws ApiAbacusException;

	/**
	 * Received payment loan.
	 *
	 * @param paymentLoan the payment loan
	 */
	void receivedPaymentLoan(String paymentLoan);

	/**
	 * Received cancel loan.
	 *
	 * @param idIbLoan the id ib loan
	 */
	void receivedCancelLoan(String idIbLoan);

	/**
	 * Received note.
	 *
	 * @param claimNoteDTO the claim note DTO
	 */
	void receivedNote(ClaimNoteDTO claimNoteDTO);
}
