/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.queue;

import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.core.TopicExchange;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The RabbitMQTopicConfig class is a configuration class that sets up the RabbitMQ topic exchange
 * and queues. It creates the necessary queues, topic exchange, and bindings for routing messages.
 * The configuration values are loaded from the application properties.
 *
 * @author nrmila
 * @see org.springframework.amqp.core.Queue
 * @see org.springframework.amqp.core.TopicExchange
 * @see org.springframework.amqp.core.Binding
 * @see org.springframework.beans.factory.annotation.Value
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @since 1.0.8
 */
@Configuration
public class RabbitMQTopicConfig {

	/** The customer queue topic. */
	@Value("${ib.rabbitmq.customer}")
	private String customerQueueTopic;

	/** The loan queue topic. */
	@Value("${ib.rabbitmq.loan}")
	private String loanQueueTopic;

	/** The loan schedules queue topic. */
	@Value("${ib.rabbitmq.loan.schedules}")
	private String loanSchedulesQueueTopic;

	/** The validate loan queue topic. */
	@Value("${ib.rabbitmq.validate.loan}")
	private String validateLoanQueueTopic;

	/** The cancel loan queue topic. */
	@Value("${ib.rabbitmq.cancel.loan}")
	private String cancelLoanQueueTopic;

	/** The exchange. */
	@Value("${ib.rabbitmq.exchange}")
	private String exchange;

	/**
	 * Creates a bean for the customer queue topic.
	 * 
	 * @return The customer queue topic.
	 */
	@Bean
	Queue customerQueueTopic() {

		return new Queue(customerQueueTopic, true);
	}

	/**
	 * Creates a bean for the loan queue topic.
	 * 
	 * @return The loan queue topic.
	 */
	@Bean
	Queue loanQueueTopic() {

		return new Queue(loanQueueTopic, true);
	}

	/**
	 * Loan schedules queue topic.
	 *
	 * @return the queue
	 */
	@Bean
	Queue loanSchedulesQueueTopic() {

		return new Queue(loanSchedulesQueueTopic, true);
	}

	/**
	 * Validate loan queue topic.
	 *
	 * @return the queue
	 */
	@Bean
	Queue validateLoanQueueTopic() {

		return new Queue(validateLoanQueueTopic, true);
	}

	/**
	 * Cancel loan queue topic.
	 *
	 * @return the queue
	 */
	@Bean
	Queue cancelLoanQueueTopic() {

		return new Queue(cancelLoanQueueTopic, true);
	}

	/**
	 * Creates a bean for the topic exchange.
	 * 
	 * @return The topic exchange.
	 */
	@Bean
	TopicExchange topicExchange() {

		return new TopicExchange(exchange);
	}

	/**
	 * Creates a binding between the customer queue topic and the topic exchange. The binding key is
	 * "queue.customer".
	 * 
	 * @param customerQueueTopic The customer queue topic.
	 * @param topicExchange The topic exchange.
	 * @return The binding between the customer queue topic and the topic exchange.
	 */
	@Bean
	Binding cusomerBindingTopic(Queue customerQueueTopic, TopicExchange topicExchange) {

		return BindingBuilder.bind(customerQueueTopic).to(topicExchange).with("key.customer");
	}

	/**
	 * Creates a binding between the loan queue topic and the topic exchange. The binding key is
	 * "queue.loan".
	 * 
	 * @param loanQueueTopic The loan queue topic.
	 * @param topicExchange The topic exchange.
	 * @return The binding between the loan queue topic and the topic exchange.
	 */
	@Bean
	Binding loanTopic(Queue loanQueueTopic, TopicExchange topicExchange) {

		return BindingBuilder.bind(loanQueueTopic).to(topicExchange).with("key.loan");
	}

	/**
	 * Validate loan binding topic.
	 *
	 * @param validateLoanQueueTopic the validate loan queue topic
	 * @param topicExchange the topic exchange
	 * @return the binding
	 */
	@Bean
	Binding validateLoanBindingTopic(Queue validateLoanQueueTopic, TopicExchange topicExchange) {

		return BindingBuilder.bind(validateLoanQueueTopic).to(topicExchange)
				.with("key.loan.validate");
	}

	/**
	 * Loan schedules binding topic.
	 *
	 * @param loanSchedulesQueueTopic the loan schedules queue topic
	 * @param topicExchange the topic exchange
	 * @return the binding
	 */
	@Bean
	Binding loanSchedulesBindingTopic(Queue loanSchedulesQueueTopic, TopicExchange topicExchange) {

		return BindingBuilder.bind(loanSchedulesQueueTopic).to(topicExchange)
				.with("key.loan.schedules");
	}

	/**
	 * Cancel loan binding topic.
	 *
	 * @param cancelLoanQueueTopic the cancel loan queue topic
	 * @param topicExchange the topic exchange
	 * @return the binding
	 */
	@Bean
	Binding cancelLoanBindingTopic(Queue cancelLoanQueueTopic, TopicExchange topicExchange) {

		return BindingBuilder.bind(cancelLoanQueueTopic).to(topicExchange).with("key.loan.cancel");
	}

}
