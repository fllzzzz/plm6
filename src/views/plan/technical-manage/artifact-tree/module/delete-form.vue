<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="部分删除"
    :wrapper-closable="false"
    size="60%"
  >
    <template #titleRight>
      <el-popconfirm
        :title="`确认删除么?`"
        @confirm="deleteItems"
      >
        <template #reference>
          <common-button type="danger" size="mini" :loading="deleteLoading" :disabled="listSelection.length === 0">
          删除
          </common-button>
        </template>
      </el-popconfirm>
    </template>
    <template #content>
      <common-table
        ref="tableRef"
        border
        :data="list"
        :max-height="260"
        style="width: 100%"
        class="table-form"
        :stripe="false"
        @selection-change="handleSelectionChange"
        return-source-data
        :showEmptySymbol="false"
      >
        <el-table-column type="selection" width="55" />
        <el-table-column label="序号" type="index" align="center" width="55" />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" min-width="100" show-overflow-tooltip />
        <el-table-column key="specification" prop="specification" label="规格" min-width="160" show-overflow-tooltip />
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" :label="`长度(mm)`" align="left">
          <template v-slot="scope">
            {{ scope.row.length ? scope.row.length.toFixed(DP.MES_ARTIFACT_L__MM) : '-' }}
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" label="数量" show-overflow-tooltip />
        <el-table-column key="netWeight" prop="netWeight" :label="`单重(kg)`" align="left" show-overflow-tooltip>
          <template v-slot="scope">
            {{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column key="surfaceArea" prop="surfaceArea" :label="`面积(㎡)`" align="left" show-overflow-tooltip />
        <el-table-column key="drawingNumber" prop="drawingNumber" label="图号" align="left" show-overflow-tooltip>
          <template v-slot="scope">
            {{ scope.row.drawingNumber || '-' }}
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'
import crudApi from '@/api/plan/technical-manage/artifact-tree'

import useVisible from '@compos/use-visible'
import { ElNotification } from 'element-plus'
import { DP } from '@/settings/config'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  list: {
    type: Array,
    default: () => []
  }
})

const tableRef = ref()

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      if (tableRef.value) {
        tableRef.value.clearSelection()
      }
    }
  },
  { deep: true, immediate: true }
)

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const listSelection = ref([])
const deleteLoading = ref(false)

function handleSelectionChange(val) {
  listSelection.value = val.map((v) => v.id)
}

async function deleteItems() {
  deleteLoading.value = true
  try {
    await crudApi.del(listSelection.value)
    deleteLoading.value = false
    handleSuccess()
  } catch (e) {
    deleteLoading.value = false
    console.log('删除构件', e)
  }
}

function handleSuccess() {
  ElNotification({ title: '删除成功', type: 'success' })
  emit('success')
  handleClose()
}

</script>
<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.el-dialog__body) {
  padding: 10px 20px;

  .el-step {
    .el-step__icon {
      width: 20px;
      height: 20px;
      font-size: 12px;
    }
    .el-step__title {
      font-size: 13px;
    }
  }
}
.tree-form {
  ::v-deep(.el-drawer__header) {
    margin-bottom: 0;
  }
}
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid #50bfff;
  margin: 5px 0;
  margin-left: 5px;
  width: 150px;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
</style>

