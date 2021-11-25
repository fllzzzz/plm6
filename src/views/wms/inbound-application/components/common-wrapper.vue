<template>
  <div class="inbound-application-container" :style="heightStyle">
    <common-header :basicClass="props.basicClass" class="header" ref="headerRef" @purchase-order-change="handleOrderInfoChange" />
    <div class="main-content">
      <slot />
    </div>
    <common-footer class="footer" :unit="props.unit" :total-value="totalValue" @submit="submit" />
  </div>
</template>

<script setup>
import { defineProps, defineEmits, ref, inject, provide } from 'vue'
import useMaxHeight from '@/composables/use-max-height'
import commonHeader from '../components/common-header.vue'
import commonFooter from '../components/common-footer.vue'

const emit = defineEmits(['purchase-order-change'])

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
  }
})

const headerRef = ref()
const form = inject('form')

const { heightStyle } = useMaxHeight({ extraBox: null, wrapperBox: null })

// function handleOrderChange(info) {
//   orderInfo.value = info
//   console.log('orderInfo', orderInfo)
// }

// 表单提交
function submit() {
  const headerValidate = headerRef.value.validate()
  console.log('form', form)
  console.log('headerValidate', headerValidate)
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
