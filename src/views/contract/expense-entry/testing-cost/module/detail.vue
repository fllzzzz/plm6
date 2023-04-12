<template>
  <div>
    <common-drawer ref="drawerRef" :title="`检测费用详情`" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="70%">
      <template #titleAfter>
        <!-- <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button> -->
      </template>
      <template #content>
        <!--表格渲染-->
        <common-table ref="tableRef" :show-empty-symbol="false" :max-height="maxHeight" :data="detailList" style="width: 100%">
          <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column align="center" key="month" prop="month" :show-overflow-tooltip="true" label="月份" width="80">
            <template #default="{ row }">
              <span>{{ row.month }}月</span>
            </template>
          </el-table-column>
          <el-table-column align="center" key="payDate" prop="payDate" :show-overflow-tooltip="true" label="支付日期" width="120">
            <template v-slot="scope">
              <span>{{ scope.row.payDate ? parseTime(scope.row.payDate, '{y}-{m}-{d}') : '-' }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" key="project" prop="project" :show-overflow-tooltip="true" label="项目" min-width="150px">
            <template v-slot="scope">
              <span>{{ projectNameFormatter(scope.row.project) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            align="center"
            key="testingFeeTypeName"
            prop="testingFeeTypeName"
            :show-overflow-tooltip="true"
            label="检测费类别"
          />
          <el-table-column align="center" key="feeAmount" prop="feeAmount" :show-overflow-tooltip="true" label="费用">
            <template v-slot="scope">
              <span>{{ toThousand(scope.row.feeAmount) }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" key="attachments" prop="attachments" :show-overflow-tooltip="true" label="附件">
            <template #header>
              <el-tooltip class="item" effect="dark" :content="`双击编号可预览文附件`" placement="top">
                <div style="display: inline-block">
                  <span>附件</span>
                  <i class="el-icon-info" />
                </div>
              </el-tooltip>
            </template>
            <template v-slot="scope">
              <!-- <upload-btn ref="uploadRef" v-model:files="scope.row.files" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.pdf,.jpg,.jpeg,.png'"/> -->
              <template v-if="scope.row.attachments && scope.row.attachments.length > 0">
                <div v-for="item in scope.row.attachments" :key="item.id">
                  <div style="cursor: pointer; color: #409eff" @dblclick="attachmentView(item)">{{ item.name }}</div>
                </div>
              </template>
            </template>
          </el-table-column>
          <el-table-column align="center" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" />
          <el-table-column align="center" label="操作">
            <template #default="{ row: { sourceRow: row } }">
              <!-- <del-btn :data="scope.row" /> -->
              <common-button type="primary" size="mini" icon="el-icon-edit" @click.stop="editClick(row)" />
              <el-popover
                v-model:visible="row.pop"
                placement="top"
                width="180"
                trigger="manual"
                @show="onPopoverShow"
                @hide="onPopoverHide"
              >
                <p>确定删除吗？</p>
                <div style="text-align: right; margin: 0">
                  <common-button size="mini" type="text" @click.stop="cancelDel(row)">取消</common-button>
                  <common-button type="primary" size="mini" @click.stop="delClick(row)">确定</common-button>
                </div>
                <template #reference>
                  <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="toDelete(row)" />
                </template>
              </el-popover>
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
      </template>
    </common-drawer>
    <m-form v-model:visible="editFormVisible" :info="itemInfo" @success="fetchDetail" @refresh="crud.toQuery" />
    <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
  </div>
</template>

<script setup>
import { getDetail, del } from '@/api/contract/expense-entry/testing-cost'
import { defineProps, defineEmits, ref, inject } from 'vue'
import { ElMessage } from 'element-plus'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import { projectNameFormatter } from '@/utils/project'

import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
import mForm from './form.vue'

// import projectCascader from '@comp-base/project-cascader.vue'

const emit = defineEmits(['update:visible', 'refresh'])
const detailList = ref([])
const pdfShow = ref(false)
const currentId = ref()
const crud = inject('crud')
const pop = ref(false)

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  },
  query: {
    type: Object,
    default: () => {}
  }
})

const editFormVisible = ref(false)
const itemInfo = ref({})

// 高度
const { maxHeight } = useMaxHeight({
  extraBox: ['.el-drawer__header'],
  wrapperBox: ['.el-drawer__body'],
  navbar: false,
  clientHRepMainH: true,
  paginate: true
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

function showHook() {
  fetchDetail()
}

async function fetchDetail() {
  let _list = []
  try {
    const { content = [], totalElements } = await getDetail({
      year: props.query?.year,
      testingFeeTypeId: props.query?.testingFeeTypeId,
      projectId: props.detailData.project?.id,
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content.map((v) => {
      v.pop = false
      return v
    })
  } catch (e) {
    console.log('获取检测费用', e)
  } finally {
    detailList.value = _list
  }
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

async function delClick(row) {
  try {
    const data = []
    data.push(row.id)
    await del(data)
    ElMessage({ type: 'success', message: '删除成功' })
    // 重新查询
    fetchDetail()
    emit('refresh')
  } catch (err) {
    console.log('delClick', err)
  }
}

function editClick(row) {
  const attachmentIds = row.attachmentFiles ? row.attachmentFiles.map((v) => v.id) : row.attachmentIds
  itemInfo.value = {
    payDate: row.payDate,
    projectId: row.project?.id,
    testingFeeTypeId: row.testingFeeTypeId,
    attachmentIds: attachmentIds,
    feeAmount: row.feeAmount,
    id: row.id,
    attachmentFiles: row.attachments,
    attachments: row.attachments,
    remark: row.remark
  }
  editFormVisible.value = true
}

function toDelete(row) {
  row.pop = true
}
function cancelDel(row) {
  row.pop = false
}

function handleDocumentClick(event) {
  pop.value = false
}

// 打开删除提示窗
function onPopoverShow() {
  setTimeout(() => {
    document.addEventListener('click', handleDocumentClick, { passive: false })
  }, 0)
}

// 隐藏删除提示窗
function onPopoverHide() {
  document.removeEventListener('click', handleDocumentClick)
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

