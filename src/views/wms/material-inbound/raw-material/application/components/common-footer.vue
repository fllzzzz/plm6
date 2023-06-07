<template>
  <div class="inbound-application-footer">
    <div class="total-box">
      <template v-if="props.showContractTotal">
        <div class="total-name">合同量</div>
        <div class="total-item">{{ `${props.contractValue || 0} ${props.contractUnit || ''}` }}</div>
      </template>
      <template v-if="props.showInboundTotal">
        <div class="total-name">已入库量</div>
        <div class="total-item">{{ `${props.inboundValue || 0} ${props.inboundUnit || ''}` }}</div>
      </template>
      <template v-if="props.showTotal">
        <div class="total-name">{{ props.totalName }}</div>
        <div class="total-item">{{ `${props.totalValue || 0} ${props.unit || ''}` }}</div>
      </template>
      <template v-if="props.showTotalAmount">
        <div class="total-name">金额合计</div>
        <div class="total-item">{{ `${props.totalAmount || 0} 元` }}</div>
      </template>
    </div>
    <div>
      <slot name="calculator" />
      <common-button
        :loading="cu.status.edit === FORM.STATUS.PROCESSING"
        type="primary"
        size="small"
        style="margin-left: 10px"
        @click="submit"
      >
        {{ btnName }}
      </common-button>
    </div>
  </div>
</template>

<script setup>
import { defineProps, defineEmits } from 'vue'
import { regExtra } from '@/composables/form/use-form'

const emit = defineEmits(['submit'])
const props = defineProps({
  permission: {
    type: Array,
    default: undefined
  },
  totalName: {
    type: String,
    default: '合计'
  },
  btnName: {
    type: String,
    default: '确认并提交'
  },
  unit: {
    type: String,
    default: ''
  },
  totalValue: {
    type: [Number, String],
    default: 0
  },
  contractUnit: {
    type: String,
    default: ''
  },
  contractValue: {
    type: [Number, String],
    default: 0
  },
  showContractTotal: {
    type: Boolean,
    default: false
  },
  inboundUnit: {
    type: String,
    default: ''
  },
  inboundValue: {
    type: [Number, String],
    default: 0
  },
  showInboundTotal: {
    type: Boolean,
    default: false
  },
  showTotal: {
    type: Boolean,
    default: true
  },
  showTotalAmount: {
    type: Boolean,
    default: false
  },
  totalAmount: {
    type: [Number, String],
    default: 0
  },
  isSubmit: {
    type: Boolean,
    default: false
  }
})

const { cu, FORM } = regExtra() // 表单

async function submit() {
  if (props.isSubmit) {
    try {
      await cu.submit()
      emit('submit')
    } catch (error) {
      console.log('入库表单提交', error)
    }
  } else {
    emit('submit')
  }
}
</script>
<style lang="scss" scoped>
.inbound-application-footer {
  display: flex;
  justify-content: space-between;
  align-items: center;
  width: 100%;
  height: 45px;
  box-shadow: 0 -1px 4px rgba(0, 21, 41, 0.08);
  background: #ffffff;
  padding: 0 25px 0 0;
  transition: 0.5s;
  z-index: 99;

  .total-box {
    display: flex;
    font-weight: bold;
    font-size: 16px;

    .total-name {
      height: 45px;
      background-color: #606266;
      display: flex;
      flex-direction: row;
      justify-content: center;
      align-items: center;
      color: #ffffff;
      box-sizing: border-box;
      padding: 0 20px;
      box-shadow: 0 -2px 2px rgba(0, 0, 0, 0.08);
    }

    .total-item {
      height: 45px;
      display: flex;
      align-items: center;
      box-sizing: border-box;
      padding: 0 20px;
      color: #606266;
    }
  }
}
</style>
