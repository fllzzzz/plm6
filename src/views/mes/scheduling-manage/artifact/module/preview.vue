<template>
  <common-dialog
    title="构件排产预览"
    custom-class="artifact-scheduling-preview"
    v-model="dialogVisible"
    width="1100px"
    top="10vh"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button @click="submitIt" :loading="submitLoading" size="mini" type="primary">保存</common-button>
    </template>
    <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <productType-base-info-columns :productType="productType" fixedWidth />
      <el-table-column :show-overflow-tooltip="true" prop="netWeight" :label="`单净重(kg)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="needSchedulingQuantity" :show-overflow-tooltip="true" label="数量" width="80" align="center" />
      <el-table-column prop="groupsName" :show-overflow-tooltip="true" label="车间>生产线>生产组" min-width="150">
        <template #default="{ row }">
          <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groupsName }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="askCompleteTime" :show-overflow-tooltip="true" label="要求完成日期" width="100">
        <template #default="{ row }">
          <span>{{ row.askCompleteTime }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { save } from '@/api/mes/scheduling-manage/artifact'
import { ElNotification } from 'element-plus'
import { defineEmits, defineProps, ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'

const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  list: {
    type: Array,
    default: () => []
  },
  productType: {
    type: [Number, String]
  }
})

const dataFormat = ref([['askCompleteTime', ['parse-time', '{y}-{m}-{d}']]])

const submitLoading = ref(false)

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.artifact-scheduling-preview',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

async function submitIt() {
  try {
    submitLoading.value = true
    const _list = props.list.map((v) => {
      return {
        askCompleteTime: v.askCompleteTime,
        factoryId: v.factoryId,
        groupsId: v.groupsId,
        productId: v.productId,
        quantity: v.needSchedulingQuantity,
        productionLineId: v.productionLine?.id,
        workshopId: v.workshop?.id
      }
    })
    await save(_list)
    ElNotification({
      title: '构件排产保存成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('保存构件排产报错', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
