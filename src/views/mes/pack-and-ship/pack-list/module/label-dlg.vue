<template>
  <common-dialog :title="`${packageInfo.packageNumber}`" v-model="dialogVisible" width="640px" :before-close="handleClose">
    <table border="1" bordercolor="#000000">
      <tr>
        <td colspan="3">{{ packageInfo.projectName }}</td>
      </tr>
      <tr>
        <td colspan="3">
          <span style="font-size: 18pt; font-weight: bold">{{ packageInfo.packageNumber }}</span>
          <span style="font-size: 16px; float: right; margin-top: 4px">{{ packageInfo.materialTypeNames }}</span>
        </td>
      </tr>
      <tr>
        <td colspan="2" style="width: 400px">总重(kg): {{ packageInfo.totalWeight }}</td>
        <td>数量：{{ packageInfo.quantity }}</td>
      </tr>
      <tr>
        <td colspan="2">打包：{{ packageInfo.packerName }} {{ packageInfo.createTime }}</td>
        <td rowspan="3">
          <div class="qr-content">
              <qrcode-vue :value="labelData.qrCode" :size="180" :margin="2" />
          </div>
        </td>
      </tr>
      <tr>
        <td rowspan="2" colspan="2" style="line-height: 1.4">备注：{{ packageInfo.remark }}</td>
      </tr>
    </table>
  </common-dialog>
</template>

<script setup>
import QrcodeVue from 'qrcode.vue'

import { computed, defineEmits, defineProps } from 'vue'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['saveSuccess', 'update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  labelData: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const packageInfo = computed(() => {
  console.log(props.labelData.packageInfo)
  return props.labelData.packageInfo || {}
})
</script>

<style lang="scss" scoped>
.qr-content {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 100%;
}
table {
  font-family: '微软雅黑';
  border-collapse: collapse;
  border: solid 1px;
  text-align: left;
  font-size: 9pt;
  color: black;
  tr td {
    padding: 0 10px;
    height: 60px;
    width: 200px;
    word-break: break-all;
  }
}
</style>
