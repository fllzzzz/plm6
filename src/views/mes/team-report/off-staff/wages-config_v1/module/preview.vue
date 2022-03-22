<template>
  <common-dialog ref="drawerRef" title="编辑预览" v-model="dialogVisible" :before-close="handleClose" width="80%">
    <template #titleRight>
      <common-button
type="primary"
size="mini"
@click.stop="submit"
:disabled="!editList.length"
:loading="submitLoading"
        >保存</common-button
      >
    </template>
    <common-table ref="tableRef" :data="editList" :dataFormat="dataFormat" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns showTeam />
      <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="140px">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="140px">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="taskQuantity" label="任务数量" align="center" width="100px" />
      <el-table-column prop="taskMete" :label="`任务量(${unitObj.unit})`" align="center" width="100px">
        <template #default="{ row }">
          <span>{{ row.taskMete }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="checkMete" :label="`核算量${checkUnitObj.UNIT}`" align="center" width="100px">
        <template #default="{ row }">
          <span>{{ row.checkMete }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="originUnitPrice" align="center" width="115px" label="原单价(元)">
        <template #default="{ row }">
          <span>{{ row.originUnitPrice }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="unitPrice" align="center" width="115px" label="当前单价(元)">
        <template #default="{ row }">
          <span>{{ row.unitPrice }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="totalAmount" label="总价(元)" align="center" width="100px">
        <template #default="{ row }">
          <span>{{ row.totalAmount }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { edit } from '@/api/mes/team-report/off-staff'
import { defineProps, defineEmits, ref, inject, computed } from 'vue'
import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  list: {
    type: Array,
    default: () => []
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const dataFormat = ref([
  ['originUnitPrice', ['to-fixed-ck', 'YUAN']],
  ['unitPrice', ['to-fixed-ck', 'YUAN']],
  ['totalAmount', ['to-fixed-ck', 'YUAN']]
])

const unitObj = inject('unitObj')
const checkUnitObj = inject('checkUnitObj')
const submitLoading = ref(false)

const editList = computed(() => {
  return props.list.filter((v) => v.unitPrice !== v.originUnitPrice)
})

async function submit() {
  try {
    submitLoading.value = true
    const _queryList = editList.value.map((v) => {
      return {
        wage: v.unitPrice,
        teamId: v.teamId,
        productType: v.productType,
        productId: v.productId
      }
    })
    await edit(_queryList)
    ElNotification({
      title: '保存成功',
      type: 'success',
      duration: 2500
    })
    emit('refresh')
    handleClose()
  } catch (error) {
    console.log('编辑保存失败', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
