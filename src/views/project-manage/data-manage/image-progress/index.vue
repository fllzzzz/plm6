<template>
  <div class="app-container">
    <div v-if="globalProject?.businessType===businessTypeEnum.INSTALLATION.V">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" @currentChange="currentChange" @handleUpload="handleUpload"/>
      </div>
      <template v-if="crud.data.length">
        <div :max-height="maxHeight">
          <div v-for="item in crud.data" :key="item.createTime">
            <span v-for="childItem in item.imageList" :key="childItem.id" style="margin-right:20px;text-align:center;width:377px;display:inline-block;margin-bottom:10px;">
              <el-image
                style="width:100%;height:251px;"
                :preview-src-list="childItem.imgSrc"
                :initial-index="1"
                :key="childItem.id"
                :src="childItem.tinyImageUrl"
                lazy
              ></el-image>
              <span>{{childItem.createTime? parseTime(childItem.createTime,'{y}-{m}-{d}'): '-'}}</span>
              <svg-icon icon-class="download" style="font-size:16px;margin-left:3px;" @click="doExport({id: childItem.id})" v-if="checkPermission(permission.download)"/>
            </span>
          </div>
        </div>
        <!--分页组件-->
        <pagination />
      </template>
      <div v-else style="color:red;margin-top:20px;">*暂无数据</div>
    </div>
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/data-manage/image-progress'
import { ref, watch } from 'vue'

import { businessTypeEnum } from '@enum-ms/contract'
import moment from 'moment'
import { imageProgressPM as permission } from '@/page-permission/project'
import { parseTime } from '@/utils/date'
import { fileDownload } from '@/utils/file'
import { downloadAttachment } from '@/api/common'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentRow = ref({})
const currentMonomer = ref({})
const uploadVisible = ref(false)
const downloadLoading = ref(false)
const { crud, CRUD } = useCRUD(
  {
    title: '形象进度',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.image-progress',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function currentChange(val) {
  currentMonomer.value = val
}

function handleUpload() {
  currentRow.value = {}
  uploadVisible.value = true
}

async function doExport(params) {
  try {
    downloadLoading.value = true
    await fileDownload(downloadAttachment, params)
  } catch (error) {
    console.log('通用导出', error)
  } finally {
    downloadLoading.value = false
  }
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProject.value.businessType === businessTypeEnum.INSTALLATION.V ? globalProjectId.value : undefined
  return !!crud.query.projectId
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  const dateArr = []
  const imageArr = []
  const originData = data.data.content
  originData.map(v => {
    if (dateArr.indexOf(moment(v.createTime).format('YYYY-MM-DD')) < 0) {
      dateArr.push(moment(v.createTime).format('YYYY-MM-DD'))
    }
  })
  if (dateArr.length) {
    for (let i = 0; i < dateArr.length; i++) {
      imageArr.push({
        createTime: dateArr[i],
        imageList: originData.filter(v => moment(v.createTime).format('YYYY-MM-DD') === dateArr[i])
      })
    }
    imageArr.map(v => {
      v.imgSrc = []
      if (v.imageList.length) {
        v.imageList.map(k => {
          v.imgSrc.push(k.imageUrl)
          k.imgSrc = [k.imageUrl]
        })
      }
    })
  }
  data.data.content = imageArr
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>
