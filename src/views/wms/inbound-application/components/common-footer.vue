<template>
  <div class="inbound-application-footer">
    <div v-show="showTotal" class="total-box">
      <div class="total-name">{{ props.totalName }}</div>
      <div class="total-item">{{ props.totalValue }}{{ props.unit }}</div>
    </div>
    <div>
      <slot name="calculator" />
      <common-button
        v-permission="props.permission"
        :loading="props.submitLoading"
        type="primary"
        size="small"
        icon="el-icon-right"
        style="margin-left: 10px"
        @click="submit"
      >
        继续
      </common-button>
    </div>
  </div>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'

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
    default: '提交'
  },
  unit: {
    type: String,
    default: ''
  },
  totalValue: {
    type: [Number, String],
    default: 0
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

const successVisible = ref(false)

function submit() {
  successVisible.value = false
  emit('submit')
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
