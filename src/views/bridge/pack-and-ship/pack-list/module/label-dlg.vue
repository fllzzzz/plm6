<template>
  <common-dialog :title="`${packageInfo.serialNumber}`" top="5vh" v-model="dialogVisible" width="600px" :before-close="handleClose">
    <div style="font-weight: bold; font-size: 16pt;color: #333;padding-bottom: 10pt;">{{ packageInfo.project.name }}</div>
    <table border="1" bordercolor="#000000">
      <tr>
        <td rowspan="2">
          <div class="qr-content">
            <qrcode-vue :value="labelData.qrCode" :size="90" :margin="2" />
          </div>
        </td>
        <td colspan="3">
          <span style="font-weight: bold; font-size: 10pt">打包单</span>
        </td>
      </tr>
      <tr>
        <td>包单号</td>
        <td colspan="2" style="font-weight: bold">{{ packageInfo.serialNumber }}</td>
      </tr>
      <tr v-if="packageInfo.productType === packTypeEnum.BOX.V || packageInfo.productType === packTypeEnum.MACHINE_PART.V">
        <td>编号</td>
        <td>材质</td>
        <td>数量</td>
        <td>重量(kg)</td>
      </tr>
      <template v-if="packageInfo.productType === packTypeEnum.BOX.V || packageInfo.productType === packTypeEnum.MACHINE_PART.V">
        <tr v-for="(item, index) in breakUpList[page - 1]" :key="index">
          <td class="col-1">{{ item.serialNumber }}</td>
          <td class="col-1">{{ item.material }}</td>
          <td class="col-1">{{ item.quantity }}</td>
          <td class="col-1">{{ item.totalWeight }}</td>
        </tr>
      </template>
    </table>
    <el-pagination
      style="text-align: center; margin-top: 10px"
      small
      layout="prev, pager, next"
      :page-count="breakUpList.length"
      v-model:current-page="page"
    ></el-pagination>
  </common-dialog>
</template>

<script setup>
import QrcodeVue from 'qrcode.vue'
import { computed, ref, defineEmits, defineProps } from 'vue'
import { bridgePackTypeEnum as packTypeEnum } from '@enum-ms/bridge'

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
  return props.labelData.packageInfo || {}
})
const page = ref(1)
const breakUpList = computed(() => {
  const _list = []
  for (var i = 0, len = packageInfo.value.list?.length; i < len; i += 11) {
    _list.push(packageInfo.value.list.slice(i, Math.min(i + 11, len)))
  }
  return _list
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
  text-align: center;
  font-size: 9pt;
  color: black;
  tr td {
    padding: 0 10px;
    height: 50px;
    width: 150px;
    word-break: break-all;
  }
}
</style>
