<template>
  <common-drawer
    ref="drawerRef"
    title="提交预览"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="delivery-detail"
    size="80%"
  >
    <template #titleRight>
      <common-button type="success" size="mini" @click="onSubmit">保存</common-button>
    </template>
    <template #content>
      <common-table :data="submitList" show-summary :summary-method="getSummaries" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomer.name" prop="monomer.name" label="单体" align="center" />
        <el-table-column key="area.name" prop="area.name" label="区域" align="center" />
        <el-table-column key="name" prop="name" label="名称" align="center" />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" />
        <el-table-column key="specification" prop="specification" label="规格" align="center" />
        <el-table-column key="measureUnit" prop="measureUnit" label="计量单位" align="center" />
        <el-table-column key="quantity" prop="quantity" label="清单数" align="center" />
        <el-table-column key="accountingUnit" prop="accountingUnit" label="核算单位" align="center" />
        <el-table-column key="mete" prop="mete" label="清单量" align="center" />
        <el-table-column key="receivingQuantity" prop="receivingQuantity" label="收货量" align="center" />
        <el-table-column key="installQuantity" prop="installQuantity" label="已安装量" align="center" />
        <el-table-column key="reportQuantity" prop="reportQuantity" label="本次安装填报" align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { installSave } from '@/api/project-manage/install-manage/handle-install'
import { ref, defineEmits, defineProps } from 'vue'

import { tableSummary } from '@/utils/el-extra'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { ElNotification } from 'element-plus'

const emit = defineEmits(['update:modelValue', 'success'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  projectId: {
    type: Number,
    default: undefined
  },
  productType: {
    type: Number,
    default: undefined
  },
  submitList: {
    type: Array,
    default: () => []
  }
})

const { visible, handleClose } = useVisible({ emit, props })

const drawerRef = ref()

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.delivery-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: ['reportQuantity']
  })
  return summary
}

async function onSubmit(val) {
  try {
    const list = []
    props.submitList.map(v => {
      list.push({
        productId: v.productId,
        reportQuantity: v.reportQuantity
      })
    })
    const submitData = {
      projectId: props.projectId,
      productType: props.productType,
      reportDTOParamList: list
    }
    await installSave(submitData)
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('安装填报保存', error)
  }
}
</script>
