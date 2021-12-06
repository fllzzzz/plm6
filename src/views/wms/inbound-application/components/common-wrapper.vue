<template>
  <div class="inbound-application-container" :style="heightStyle">
    <common-header :basic-class="props.basicClass" :edit="props.edit" class="header" ref="headerRef" @purchase-order-change="handleOrderInfoChange" />
    <div class="main-content">
      <slot />
    </div>
    <common-footer
      class="footer"
      :unit="props.unit"
      :total-name="props.totalName"
      :total-value="props.totalValue"
      :show-total="props.showTotal"
      :btn-name="props.btnName"
      @submit="submit"
    />
    <confirm-dialog v-model="previewVisible" :basic-class="props.basicClass" />
  </div>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'

import useMaxHeight from '@/composables/use-max-height'
import commonHeader from '@/views/wms/inbound-application/components/common-header.vue'
import commonFooter from '@/views/wms/inbound-application/components/common-footer.vue'
import confirmDialog from './confirm-dialog.vue'

const emit = defineEmits(['purchase-order-change', 'submit'])

const props = defineProps({
  basicClass: {
    type: Number
  },
  validate: {
    type: Function
  },
  unit: {
    type: String,
    default: ''
  },
  totalValue: {
    type: [Number, String],
    default: 0
  },
  totalName: {
    type: String,
    default: '合计'
  },
  btnName: {
    type: String,
    default: '下一步'
  },
  showTotal: {
    type: Boolean,
    default: true
  },
  edit: {
    type: Boolean,
    default: false
  }
})

const headerRef = ref()
const previewVisible = ref(false)

let heightCfg = {}
if (props.edit) {
  heightCfg = { mainBox: '.raw-mat-inbound-application-record-form', extraBox: ['.el-drawer__header'], wrapperBox: ['.el-drawer__body'], clientHRepMainH: true, navbar: false }
} else {
  heightCfg = { extraBox: null, wrapperBox: null }
}

const { heightStyle } = useMaxHeight(heightCfg)

// 表单提交（预览）
async function submit() {
  const headerValidate = await headerRef.value.validate()
  let formValidate = true
  if (typeof props.validate === 'function') {
    formValidate = await props.validate()
  }
  if (headerValidate && formValidate) {
    previewVisible.value = true
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
    padding: 0 20px 0px 20px;
  }
}
</style>
