<template>
  <div class="inbound-application-container" :style="heightStyle">
    <common-header :basicClass="props.basicClass" class="header" ref="headerRef" @purchase-order-change="handleOrderInfoChange" />
    <div class="main-content">
      <slot />
    </div>
    <common-footer
      class="footer"
      :unit="props.unit"
      :total-name="props.totalName"
      :permission="props.permission"
      :total-value="props.totalValue"
      :submit-loading="props.submitLoading"
      :show-total="props.showTotal"
      @submit="submit"
    />
  </div>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import useMaxHeight from '@/composables/use-max-height'
import commonHeader from '../components/common-header.vue'
import commonFooter from '../components/common-footer.vue'

const emit = defineEmits(['purchase-order-change', 'submit'])

const props = defineProps({
  basicClass: {
    type: Number
  },
  unit: {
    type: String,
    default: ''
  },
  totalValue: {
    type: [Number, String],
    default: 0
  },
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
    default: '提交'
  },
  submitLoading: {
    type: Boolean,
    default: false
  },
  showTotal: {
    type: Boolean,
    default: true
  }
})

const headerRef = ref()

const { heightStyle } = useMaxHeight({ extraBox: null, wrapperBox: null })

// 表单提交（下一步）
async function submit() {
  const headerValidate = await headerRef.value.validate()
  if (headerValidate) {
    emit('submit')
  }
}

// 订单详情变更
function handleOrderInfoChange(val) {
  emit('purchase-order-change', val)
}
</script>

<style lang="scss" scoped>
.inbound-application-container {
  position: relative;
  .header {
    padding: 15px 20px 10px 10px;
  }
  .footer {
    position: absolute;
    bottom: 0;
    left: 0;
  }

  .main-content {
    padding: 0 20px 10px 20px;
  }
}
</style>
