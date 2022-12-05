<template>
  <div v-show="!layingOffRow?.id">
    <div class="my-code">点击钻孔配置查看详情</div>
  </div>
  <div v-show="layingOffRow?.id">
    <!--表格渲染-->
    <el-card class="box-card team-card">
      <template v-slot:header class="clearfix card-header">
        <div style="display: flex; align-items: center; justify-content: space-between">
          <span style="display: flex; align-items: center">
            <span>孔径详情列表</span>
          </span>
          <common-button
            size="mini"
            style="float: right; padding: 6px 10px; margin-bottom: 0px"
            type="primary"
            icon="el-icon-plus"
            @click="toAdd"
          >
            新增
          </common-button>
        </div>
      </template>
      <common-table
        ref="tableRef"
        :data="layingOffRow?.configLinkDTOS"
        highlight-current-row
        :empty-text="'暂无数据'"
        :max-height="maxHeight + 92"
        style="width: 100%"
        row-key="id"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" label="孔径数值范围（毫米）" align="center" min-width="180px">
          <template #default="{ row }">
            <span>{{ row.minBoreDiameter }}</span>
            <span> ~ </span>
            <span>{{ row.maxBoreDiameter }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="unitPrice" label="单价（元/个）" align="center" width="130px" />
        <!--编辑与删除-->
        <el-table-column label="操作" width="130px" align="center" fixed="right">
          <template #default="{ row }">
            <common-button class="filter-item" size="mini" type="primary" icon="el-icon-edit" @click.stop="toEdit(row)" />
            <el-popover v-model:visible="row.deleteBtn" placement="top" width="180" trigger="manual">
              <p>确认删除本条数据吗？</p>
              <div style="text-align: right; margin: 0">
                <common-button size="mini" type="text" @click="cancelDelete">取消</common-button>
                <common-button type="primary" size="mini" @click="handleDelete">确定</common-button>
              </div>
              <template #reference>
                <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="toDelete(row)" />
              </template>
            </el-popover>
          </template>
        </el-table-column>
      </common-table>
    </el-card>
    <!-- 表单 -->
    <m-form
      v-model:visible="formVisible"
      :isEdit="isEdit"
      :form-data="editRow"
      :layingOffRowId="layingOffRow?.id"
      @refresh="refreshCutConfig"
    />
  </div>
</template>

<script setup>
import { delGet } from '@/api/mes/production-config/drill-detail'
import { defineExpose, ref, defineProps, defineEmits, watch, computed, inject } from 'vue'
import mForm from './module/form'

const emit = defineEmits(['refresh'])
const isEdit = ref(false)
const formVisible = ref(false)
const editRow = ref({})

// 是否默认初始行
const defaultFirstRow = ref(false)

const maxHeight = inject('maxHeight')

const props = defineProps({
  layingOffRow: {
    type: Object,
    default: () => {}
  }
})

const layingOffId = computed(() => {
  return props.layingOffRow && props.layingOffRow.id
})

watch(
  () => layingOffId,
  (val) => {
    if (val.value) {
      defaultFirstRow.value = true
    }
  },
  { deep: true, immediate: true }
)

function refresh() {
  defaultFirstRow.value = false
}
function toEdit(row) {
  editRow.value = row
  isEdit.value = true
  formVisible.value = true
}

function toDelete(row) {
  editRow.value = row
  row.deleteBtn = true
}

// 取消删除
function cancelDelete() {
  editRow.value.deleteBtn = false
}

// 刷新切割配置
function refreshCutConfig() {
  emit('refresh')
}

// 确认删除
async function handleDelete() {
  editRow.value.deleteBtn = false
  try {
    await delGet([editRow.value.id])
    refreshCutConfig()
  } catch (error) {
    console.log('删除孔径详情失败', error)
  }
}

function toAdd() {
  isEdit.value = false
  formVisible.value = true
}

defineExpose({
  toAdd,
  refresh
})
</script>
