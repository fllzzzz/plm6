<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="构件绑定列表"
    :wrapper-closable="false"
    size="80%"
    custom-class="contract-change1"
  >
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="150px">
        <div style="display:flex;">
          <div>
            <project-cascader v-model="form.projectId" clearable class="filter-item" style="width: 270px;margin-bottom:10px;" placeholder="项目搜索" />
            <div>
              <monomer-select
                ref="monomerSelectRef"
                v-model="form.monomerId"
                style="width: 270px;"
                :project-id="form.projectId"
                class="filter-item"
              />
            </div>
            <div style="margin:10px 0;">
              <el-input
                v-model="form.fileName"
                placeholder="构件类型搜索"
                class="filter-item"
                style="width: 270px !important;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件类型搜索"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件名称搜索"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件编号"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件规格"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="form.fileName"
                placeholder="构件材质"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="text-align:right;">
              <common-button class="filter-item" size="small" type="success" icon="el-icon-search" @click.stop="fetchList">搜索</common-button>
              <common-button
                class="filter-item"
                size="small"
                type="warning"
                icon="el-icon-refresh"
                @click.stop="resetSubmit"
              >
                重置
              </common-button>
            </div>
          </div>
          <div style="flex:1;padding-left:10px;">
            <common-table
              ref="detailRef"
              border
              :data="[]"
              :max-height="300"
              style="width: 100%;"
              class="table-form"
              return-source-data
            >
              <el-table-column label="序号" type="index" align="center" width="50" />
              <el-table-column prop="depositBank" label="项目" align="center" show-overflow-tooltip/>
              <el-table-column prop="account" label="单体" align="center" show-overflow-tooltip/>
              <el-table-column prop="account" label="编号" align="center" show-overflow-tooltip/>
              <el-table-column prop="depositBank" label="规格" align="center" show-overflow-tooltip />
              <el-table-column prop="depositBank" label="材质" align="center" show-overflow-tooltip />
              <el-table-column label="操作" align="center">
                <template v-slot="scope">
                  <common-button size="small" class="el-icon-view" type="danger">解绑</common-button>
                </template>
              </el-table-column>
            </common-table>
             <!-- 分页 -->
            <el-pagination
              :total="total"
              :current-page="queryPage.pageNumber"
              :page-size="queryPage.pageSize"
              style="margin-top: 8px"
              layout="total, prev, pager, next, sizes"
              @size-change="handleSizeChange"
              @current-change="handleCurrentChange"
            />
          </div>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, computed, ref, watch } from 'vue'
import useVisible from '@compos/use-visible'

import usePagination from '@compos/use-pagination'
import projectCascader from '@comp-base/project-cascader.vue'
import monomerSelect from '@/components-system/plan/monomer-select'

const props = defineProps({
  currentMonomer: {
    type: Object,
    default: () => {}
  },
  globalProject: {
    type: Object,
    default: () => {}
  },
  dataType: {
    type: [String, Number],
    default: undefined
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const defaultForm = {
  fileName: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const rules = {
  fileName: { required: true, message: '请输入文件命名', trigger: 'blur' }
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// watch(
//   () => visible.value,
//   (val) => {
//     if (val) {
//       form.value.fileName = undefined
//     }
//   },
//   { deep: true, immediate: true }
// )

const carryParam = computed(() => {
  return {}
  // return props.currentRow.id ? { id: props.currentRow.id, fileName: form.value.fileName } : { projectId: props.globalProject.id, monomerId: props.currentMonomer.id, dataType: props.dataType, fileName: form.value.fileName }
})

function handleSuccess() {
  emit('success')
  handleClose()
}

function fetchList() {

}

function resetSubmit() {

}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
