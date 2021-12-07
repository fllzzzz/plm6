<template>
  <common-dialog
    :title="`${component.name}-${component.serialNumber}`"
    v-model:visible="dialogVisible"
    width="640px"
    :before-close="handleClose"
  >
    <table border="1" bordercolor="#000000">
      <tr>
        <!--项目-->
        <td colspan="2">{{ component.projectName }}</td>
        <!--单体名称-->
        <td>{{ component.monomerName }}</td>
      </tr>
      <tr>
        <td style="font-size: 25pt; font-weight: bold" colspan="3">{{ component.serialNumber }}</td>
      </tr>
      <tr>
        <td>名称： {{ component.name }}</td>
        <td colspan="2">规格： {{ component.specification }}</td>
      </tr>
      <tr>
        <td>数量：{{ component.quantity }}</td>
        <td>长度(m): {{ component.length }}</td>
        <td>单重(kg): {{ component.weight }}</td>
      </tr>
      <tr>
        <td colspan="2">{{ component.areaName ? '区域：' + component.areaName : '' }}</td>
        <td rowspan="3">
          <div class="qr-content">
            <qrcode-vue :value="labelData.qrCode" :size="180" :margin="2" />
          </div>
        </td>
      </tr>
      <tr>
        <td colspan="2">{{ labelData.productionLineName }}</td>
      </tr>
      <tr>
        <td colspan="2">{{ labelData.manufacturerName }}</td>
      </tr>
    </table>
  </common-dialog>
</template>

<script setup>
import QrcodeVue from 'qrcode.vue'

import { computed, defineEmits, defineProps } from 'vue'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible'])
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

const component = computed(() => props.labelData.component || {})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
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
