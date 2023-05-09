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
    size="90%"
    custom-class="already-form"
  >
    <template #titleAfter>
      <el-tag>{{planProcessTypeEnum.VL[currentRow.processType]}}</el-tag>
    </template>
    <template #content>
        <div style="display:flex;">
          <div>
            <project-cascader v-model="query.projectId" clearable class="filter-item" style="width: 270px;margin-bottom:10px;" placeholder="项目搜索" />
            <div>
              <monomer-select
                ref="monomerSelectRef"
                v-model="query.monomerId"
                style="width: 270px;"
                :default="false"
                :project-id="query.projectId"
                class="filter-item"
              />
            </div>
             <div style="margin:10px 0;">
              <common-select
                v-model="query.structureClassId"
                :options="structureClassList"
                type="other"
                clearable
                :data-structure="{ key: 'id', label: 'name', value: 'id' }"
                class="filter-item"
                style="width: 270px"
                placeholder="构件类型"
              />
            </div>
           <div style="margin-bottom:10px;">
              <el-input
                v-model="query.name"
                placeholder="构件名称搜索"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="query.serialNumber"
                placeholder="构件编号"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="query.specification"
                placeholder="构件规格"
                class="filter-item"
                style="width: 270px;"
                size="small"
                clearable
              />
            </div>
            <div style="margin-bottom:10px;">
              <el-input
                v-model="query.material"
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
              :data="list"
              :max-height="maxHeight-80"
              style="width: 100%;"
              class="table-form"
              :data-format="dataFormat"
            >
              <el-table-column label="序号" type="index" align="center" width="50" />
              <el-table-column prop="project" label="项目" align="left" min-width="150" show-overflow-tooltip v-if="!currentRow.boolSingleProject" />
              <el-table-column prop="monomerName" label="单体" align="left" show-overflow-tooltip/>
              <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip/>
              <el-table-column prop="name" label="构件名称" align="center" show-overflow-tooltip/>
              <el-table-column prop="structureClassName" label="构件类型" align="center" show-overflow-tooltip/>
              <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip />
              <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip width="80" />
              <el-table-column label="操作" align="center" width="80">
                <template v-slot="scope">
                  <common-button size="mini" type="danger" @click="deleteItem(scope.row)" v-permission="permission.unbind">解绑</common-button>
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
    </template>
  </common-drawer>
</template>

<script setup>
import { bindStructureList, unbindStructure } from '@/api/plan/technical-data-manage/process'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'
import useVisible from '@compos/use-visible'

import { planProcessTypeEnum } from '@enum-ms/plan'
import { ElMessage } from 'element-plus'
import { planProcessListPM as permission } from '@/page-permission/plan'
import useMaxHeight from '@compos/use-max-height'

import usePagination from '@compos/use-pagination'
import projectCascader from '@comp-base/project-cascader.vue'
import monomerSelect from '@/components-system/plan/monomer-select'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const query = ref({})
const list = ref([])

const tableLoading = ref(false)
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const dataFormat = ref([
  ['project', 'parse-project']
])

const structureClassList = inject('structureClassList')
const drawerRef = ref()

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.already-form',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

watch(
  () => visible.value,
  (val) => {
    if (val) {
      for (const i in query.value) {
        if (props.currentRow.boolSingleProject) {
          query.value[i] = props.currentRow.projectId
        } else {
          query.value[i] = undefined
        }
      }
      fetchList()
    }
  },
  { deep: true, immediate: true }
)

async function deleteItem(row) {
  try {
    await unbindStructure({
      processFileId: props.currentRow.id,
      details: [{
        monomerId: row.monomerId,
        serialNumber: row.serialNumber
      }]
    })
    ElMessage.success('构件：' + row.serialNumber + '解绑成功')
    fetchList()
    emit('success')
  } catch (error) {
    console.log('绑定失败', error)
  }
}

async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await bindStructureList({ ...query.value, processFileId: props.currentRow.id, ...queryPage })
    setTotalPage(totalElements)
    _list = content
  } catch (error) {
    console.log('构件绑定明细', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

function resetSubmit() {
  for (const i in query.value) {
    if (props.currentRow.boolSingleProject) {
      query.value[i] = props.currentRow.projectId
    } else {
      query.value[i] = undefined
    }
  }
  fetchList()
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
