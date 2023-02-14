<template>
  <div class="requisitions-application-container" :style="heightStyle">
    <div class="main-content">
      <slot />
    </div>
    <common-footer
      v-if="props.showFooter"
      class="footer"
      :unit="props.unit"
      :total-name="props.totalName"
      :total-value="props.totalValue"
      :show-total="props.showTotal"
      :btn-name="props.btnName"
      @submit="submit"
    />
    <confirm-dialog v-model="previewVisible" />
  </div>
</template>

<script setup>
import { defineProps, defineExpose, ref } from 'vue'

import useMaxHeight from '@/composables/use-max-height'
import commonFooter from './common-footer.vue'
import confirmDialog from './confirm-dialog.vue'

const props = defineProps({
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
  showFooter: {
    type: Boolean,
    default: true
  },
  edit: {
    type: Boolean,
    default: false
  }
})

const previewVisible = ref(false)

const { heightStyle } = useMaxHeight({
  ainBox: '.requisitions-application-record-form',
  extraBox: ['.el-drawer__header', '.requisitions-application-select'],
  wrapperBox: ['.el-drawer__body'],
  clientHRepMainH: true,
  navbar: false
})

// 表单提交（预览）
async function submit() {
  let formValidate = true
  if (typeof props.validate === 'function') {
    formValidate = await props.validate()
  }
  previewVisible.value = formValidate
}

defineExpose({
  submit
})
</script>

<style lang="scss" scoped>
.requisitions-application-container {
  position: relative;
  .footer {
    position: absolute;
    bottom: -1px;
    left: 0;
  }
  .main-content {
    padding: 0 20px 0px 20px;
  }
}
</style>
