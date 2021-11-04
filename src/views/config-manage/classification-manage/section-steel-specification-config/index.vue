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
              style="margin-right: 5px"
              icon="el-icon-view"
              @click="showSpec(scope.row)"
              >查看</common-button
            >
            <el-popover placement="bottom" :title="``" trigger="click" width="100">
              <!-- <upload-btn
                    v-permission="permission.import"
                    class="upload-btn"
                    :action="fileUploadApi"
                    v-model:files="scope.row.files"
                    :has-before-confirm="true"
                    :before-message="'确认更换当前清单吗？'"
                    :file-classify="fileClassificationEnum.SECTION_ATT.V"
                    btn-name="GB-98"
                    :icon="'el-icon-upload2'"
                    :size="'mini'"
                    :limit="1"
                    :show-file-list="false"
                    btn-type="info"
                    style="margin-bottom: 5px"
                    @change="filesChange(scope.row)"
                  />
                  <upload-btn
                    v-permission="permission.import"
                    class="upload-btn"
                    :action="fileUploadApi"
                    v-model:files="scope.row.files"
                    :has-before-confirm="true"
                    :before-message="'确认更换当前清单吗？'"
                    :file-classify="fileClassificationEnum.SECTION_ATT.V"
                    btn-name="GB-82"
                    :icon="'el-icon-upload2'"
                    :size="'mini'"
                    :limit="1"
                    :show-file-list="false"
                    btn-type="info"
                    @change="filesChange(scope.row)"
                  /> -->
              <template #reference>
                <common-button size="mini" icon="el-icon-upload2" type="warning">上传</common-button>
              </template>
            </el-popover>
            <el-popover placement="bottom" :title="``" trigger="click" width="100">
              <common-button
                class="download-btn"
                :loading="scope.row.downloadLoading"
                size="mini"
                style="margin-bottom: 5px"
                type="info"
                icon="el-icon-download"
                @click="downloadFile(scope.row, scope.row.id)"
                >GB-98</common-button
              >
              <common-button
                class="download-btn"
                :loading="scope.row.downloadLoading"
                size="mini"
                style="margin-left: 0px"
                type="info"
                icon="el-icon-download"
                @click="downloadFile(scope.row, scope.row.id)"
                >GB-82</common-button
              >
              <template #reference>
                <common-button size="mini" icon="el-icon-upload2" type="success" style="margin-left: 5px">下载</common-button>
              </template>
            </el-popover>
            <!-- <common-button :loading="scope.row.downloadLoading" size="mini" style="margin-left:5px;" type="success" icon="el-icon-download" @click="downloadFile(scope.row,scope.row.id)">下载</common-button> -->
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
// import { mapGetters } from '@/store/lib'
// import { fileClassificationEnum } from '@enum-ms/file'
import { isNotBlank, isBlank } from '@data-type/index'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
// import uploadBtn from '@comp/FileUpload/UploadBtn'
import { ElMessage, ElNotification, ElCheckTag } from 'element-plus'
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
      tree.forEach(node => {
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

async function filesChange(row) {
  try {
    if (row.files && row.files.length > 0) {
      row.attachmentId = row.files[0].id
      await crudApi.edit(row)
      ElNotification({ title: '更新成功', type: 'success', duration: 2500 })
      crud.toQuery()
    }
  } catch (error) {
    ElNotification({ title: '更新失败', type: 'error', duration: 2500 })
  }
}

function showSpec(row) {
  currentRow.value = row
  specsVisible.value = true
}

async function changeStandard(row, standardId) {
  if (row.standardId === standardId) return
  try {
    await setStandard(row.standardId, standardId)
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
.upload-btn {
  width: 72px;
}
.download-btn {
  width: 84px;
}
.check-tag + .check-tag {
  margin-left: 10px;
}
</style>
