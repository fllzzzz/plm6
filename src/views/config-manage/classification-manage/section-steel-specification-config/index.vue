<template>
  <div class="app-container section-steel">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="tableList"
      row-key="id"
      style="width: 100%"
      :max-height="maxHeight"
      default-expand-all
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="型材编号"
        align="left"
        width="150"
      />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        :show-overflow-tooltip="true"
        prop="name"
        label="型材名称"
        align="left"
        width="250"
      />
      <el-table-column
        v-if="columns.visible('standardId')"
        key="standardId"
        :show-overflow-tooltip="true"
        prop="standardId"
        label="执行标准"
        align="left"
      >
        <template v-slot="scope">
          <template v-if="scope.row.isLeaf">
            <el-check-tag
              class="check-tag"
              v-for="sd in standard"
              :key="sd.id"
              type="success"
              :checked="scope.row.standardId === sd.id"
              @click="changeStandard(scope.row, sd.id)"
            >
              {{ sd.name }}
            </el-check-tag>
          </template>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('attachmentId')"
        key="attachmentId"
        :show-overflow-tooltip="true"
        prop="attachmentId"
        label="操作"
        align="left"
      >
        <template v-slot="scope">
          <div v-if="scope.row.isLeaf" style="display: flex; justify-content: flex-start">
            <common-button
              v-permission="permission.detail"
              size="mini"
              type="primary"
              icon="el-icon-view"
              @click="showSpec(scope.row)"
              >查看</common-button
            >
            <el-popover placement="right" :title="``" trigger="click" :width="150">
              <excel-resolve-preview-button
                v-for="sd in standard"
                :key="`import_${sd.id}`"
                class="resolve-btn"
                :btn-name="sd.name"
                :template="sectionSteelSpecITmpl"
                :title="`${scope.row.name}：${sd.name}`"
                :submitFn="(data) => sectionSteelSpecITmpl.submit(data, scope.row, sd)"
                @success="handleUploadSuccess"
              />
              <template #reference>
                <common-button size="mini" icon="el-icon-upload2" type="warning">上传</common-button>
              </template>
            </el-popover>
            <el-popover placement="right" :title="``" trigger="click" :width="150">
              <excel-export-button
                v-for="sd in standard"
                :key="`export_${sd.id}`"
                class="resolve-btn"
                :btn-name="sd.name"
                :template="sectionSteelSpecETmpl"
                :title="`${scope.row.name}：${sd.name}`"
                :filename="`${scope.row.name}_${sd.name}规格清单`"
                :params="{ sectionSteelId: scope.row.id, standardId: sd.id }"
              />
              <template #reference>
                <common-button size="mini" icon="el-icon-download" type="success">下载</common-button>
              </template>
            </el-popover>
          </div>
        </template>
      </el-table-column>
    </common-table>
    <m-form />
    <!-- 查看规格清单 -->
    <spec-detail v-model:visible="specsVisible" />
  </div>
</template>

<script setup>
import crudApi, { addStandard, getStandard, setStandard } from '@/api/config/classification-manage/section-steel-spec-config'
import { provide, ref, computed } from 'vue'
import { isNotBlank, isBlank } from '@data-type/index'
import sectionSteelSpecITmpl from '@/utils/excel/import-template/config/section-steel-spec-template'
import sectionSteelSpecETmpl from '@/utils/excel/export-template/config/section-steel-spec-template'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import ExcelResolvePreviewButton from '@comp-common/excel-resolve-preview-button/index.vue'
import ExcelExportButton from '@comp-common/excel-export-button/index.vue'
import { ElMessage, ElCheckTag } from 'element-plus'
import mHeader from './module/header'
import mForm from './module/form'
import specDetail from './detail/index.vue'

const permission = {
  get: ['config_class_sectionSteelLibrary:get'],
  add: ['config_class_sectionSteelLibrary:add'],
  edit: ['config_class_sectionSteelLibrary:edit'],
  del: ['config_class_sectionSteelLibrary:del']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '型材',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: crudApi.get, add: addStandard },
    hasPagination: false,
    dataPath: null
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

const specsVisible = ref(false)
const currentRow = ref({})
const standard = ref([]) // 国标数组
const standardMap = ref(new Map()) // id:name 方便展示
// const { fileUploadApi } = mapGetters('fileUploadApi')

const tableList = computed(() => {
  if (!crud.query || isBlank(crud.query.name)) {
    return crud.data
  }
  return crud.data.filter((v) => v.name.indexOf(crud.query.name) > -1)
})

provide('standard', standard)
provide('sectionSteel', currentRow)

fetchStandard()

CRUD.HOOK.handleRefresh = (crud, res) => {
  if (isNotBlank(res.data)) {
    const iterateTree = (tree) => {
      tree.forEach((node) => {
        node.downloadLoading = false
        if (isNotBlank(node.children)) {
          iterateTree(node.children)
        } else {
          delete node.children
          node.isLeaf = true
        }
      })
    }
    iterateTree(res.data)
  }
}

// 显示型材规格详情
function showSpec(row) {
  currentRow.value = row
  specsVisible.value = true
}

// 获取国标
async function fetchStandard() {
  try {
    standard.value = (await getStandard()) || []
    standardMap.value = new Map()
    standard.value.forEach((v) => {
      standardMap.value.set(v.id, v.name)
    })
  } catch (error) {
    console.log('获取国标', error)
  }
}

// 处理上传成功
function handleUploadSuccess() {
  crud.refresh()
  fetchStandard()
}

async function changeStandard(row, standardId) {
  if (row.standardId === standardId) return
  try {
    await setStandard(standardId, row.id)
    ElMessage({
      message: `【${row.name}】默认国标切换为【${standardMap.value.get(standardId)}】`,
      type: 'success'
    })
    // 不刷新页面只更新标准
    row.standardId = standardId
    // 重新拉取国标状态，可及时更新国标“可删除”状态
    fetchStandard()
  } catch (error) {
    console.log('切换国标', error)
  }
}
</script>

<style lang="scss" scoped>
.el-check-tag {
  font-weight: 300;
}
.section-steel .box-card {
  width: 100%;
}
.resolve-btn {
  width: 100%;
}
.resolve-btn + .resolve-btn {
  margin-top: 5px;
  margin-left: 0;
}
.download-btn {
  width: 84px;
}
.check-tag + .check-tag {
  margin-left: 10px;
}
</style>
